{-# LANGUAGE DeriveFunctor, TupleSections, FlexibleInstances, ExistentialQuantification, OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-} -- holy language extensions batman.
module Main where

import qualified Control.Monad.Trans.Either as E
import Data.List.Split (chunksOf)
import Data.Text.ICU.Convert
import qualified Data.HashMap.Strict as M
import Data.Time.Clock (diffUTCTime,getCurrentTime,NominalDiffTime)
import Data.Time.LocalTime
import qualified Text.Search.Sphinx.Types as Sphinx
import Text.Search.Sphinx.Types (QueryResult(..))
import Text.Search.Sphinx hiding (sortBy,mode)
import Text.Search.Sphinx.ExcerptConfiguration (ExcerptConfiguration)
import Data.Text.ICU hiding (compare,pattern)
import Data.Hashable (Hashable)

import Data.Maybe (listToMaybe,fromJust,isJust,mapMaybe)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB
import PFEG
import PFEG.Types
import PFEG.SQL
import PFEG.Common
import PFEG.Context

import Prelude hiding (log)

import Data.List (sortBy,intercalate)

import Data.Function (on)

import Data.Iteratee.Base
import qualified Data.Iteratee as I
import Data.Iteratee.IO

import System.IO

import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent (ThreadId,killThread,forkIO)
import Control.Exception (bracket)

import Control.Monad.State.Strict
import Control.Monad.Reader

import Database.HDBC
import Database.HDBC.MySQL

import Graphics.Vty.Terminal

import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Text.Groom
import PFEG.Configuration
import qualified ReadArgs as RA
import qualified Data.HashSet as Set

import qualified PFEG.Pattern as Pat

data DBAction = DBWrite Statement [[SqlValue]]
              | Log Text Corpus
              | Commit
              | Shutdown
              | ShutdownACK

dbthread :: Connection -> TBChan DBAction -> IO ()
dbthread conn chan = go
    where go = do action <- atomically $ readTBChan chan
                  perform action
                  case action of
                       Shutdown -> return ()
                       _ -> go
          perform :: DBAction -> IO ()
          perform (DBWrite stmt sql) = executeMany stmt sql
          perform Shutdown = atomically $ writeTBChan chan ShutdownACK
          perform ShutdownACK = putStrLn "WARNING: dbthread got its own ACK. That's stupid!"
          perform Commit = commit conn
          perform (Log event (name,fp)) = do
              logStatement <- prepare conn insertAction
              t <- getCurrentTime
              tz <- getCurrentTimeZone
              let t' = show $ utcToLocalTime tz t
              void $ execute logStatement [toSql event, toSql name, toSql fp, toSql t']

main :: IO ()
main = do
    (mode :: String, configFile :: FilePath) <- RA.readArgs
    withRTSSignalsBlocked $
      bracket (do putStrLn "Initializing…"
                  terminal_handle >>= hide_cursor
                  configAttempt <- configurePFEG mode configFile
                  case configAttempt of
                       (Left err) -> error $ "Initialization failed: "++ groom err
                       (Right config) -> return config)
              (\session -> do
                  putStrLn "Shutting down…"
                  terminal_handle >>= show_cursor
                  deinitialize session)
              (\session -> do
                  putStrLn "Running…"
                  process session)

getStatsLog :: PFEGConfig -> Maybe Handle
getStatsLog PFEGConfig { pfegMode = Learn { statLog = rl } } = Just rl
getStatsLog _ = Nothing

process :: PFEGConfig -> IO ()
process session = do
    tchan <- atomically $ newTBChan 2
    _dbthread_id <- forkIO $ dbthread (database session) tchan
    logChan <- atomically newTChan
    _debLthread_id <- forkIO . forever $ pfegLogger (debugLog session) (getStatsLog session) logChan
    let log = atomically . writeTChan logChan
        res = cardRegexes session
    case pfegMode session of
        Record{ corpora = cs } -> do
          s <- prepare (database session) insertText
          let it = documentIteratee (recordDocsF tchan s)
          workOnCorpora "record" tchan (documentI res)it session () cs
        Learn{ corpora = cs } -> do
          chan <- newChan
          void $ forkIO (evalPFEG (forever $ learnLogger log chan) () session)
          let it = itemIteratee (getSentenceItems (`elem` targets session)) (learnF qg chan)
              qg = generateCohortQueries (sphinxIndex session) $ makeCohorts (targets session) (matchPatterns session)
          workOnCorpora "match" tchan (documentI res) it session 0 cs
        Predict { corpora = cs } -> do
          let it = itemIteratee (getSentenceItems (`elem` targets session)) (predictF qg log)
              qg = generateCohortQueries (sphinxIndex session) $ makeCohorts (targets session) (matchPatterns session)
          workOnCorpora "predict" tchan (documentI res) it session initialScore cs
    putStrLn "Waiting for DB…"
    atomically $ writeTBChan tchan Shutdown
    atomically $ do
        r <- tryPeekTBChan tchan
        unless (dbthreadIsDone r) retry
        where dbthreadIsDone (Just ShutdownACK) = True
              dbthreadIsDone _ = False

-- | debugging function for the REPL
generateItems :: Regexes -> Converter -> FilePath -> IO [Item Text]
generateItems res conv fp = do
    let ig = getSentenceItems (`elem` T.words "auf in am")
    liftM concat $
        I.run (I.joinIM $ enumFile 65536 fp $ I.joinI $ (I.mapChunks (toUnicode conv) I.><> I.convStream (documentI res)) (I.joinI $ I.mapChunks ig I.getChunks))

type Logger = LogMessage -> IO ()

newtype Cohort a = Cohort { extractCohort :: (Pat.MatchPattern,[(Text,a)]) }
                   deriving (Functor,Show)

-- | From a list of targets and patterns, generate all cohorts, sorted by
-- pattern weight. 
makeCohorts :: [Text] -> [Pat.MatchPattern] -> [Cohort ()]
makeCohorts ts = map (\p -> Cohort . (p,) $ zip ts (repeat ())) . sortBy (flip compare `on` Pat.weight)

data Prediction = Baseline { pattern :: !Pat.MatchPattern }
                | Winner { predictedWord :: !Text, hits :: !Int, pattern :: !Pat.MatchPattern } deriving Show

winner :: Cohort Int -> Prediction
winner (Cohort (pat,[])) = Baseline pat
winner (Cohort (pat,counts)) = case head . sortBy (flip compare `on` snd) $ counts of
                                 (_,0) -> Baseline pat
                                 (t,c) -> Winner t c pat

-- THIIIISS IIIIS… really slow.
attack :: Cohort Query -> PFEG st Prediction
attack (Cohort (p,qs)) = do
    session <- ask
    (counts,warning) <- liftIO $ liftM getCounts $ runQueries (searchConf session) (map snd qs)
    -- TODO log the warning
    return $ winner $ Cohort (p,zipWith (\ (t,_) r -> (t,r)) qs counts)

getCounts :: Sphinx.Result [QueryResult] -> ([Int],Text)
getCounts (Sphinx.Ok a)        = (map total a,T.empty)
getCounts (Sphinx.Warning t a) = (map total a,t)
getCounts (Sphinx.Error err t) = ([],T.intercalate ": " [T.pack (show err),t])
getCounts (Sphinx.Retry t)     = ([],t)

makeQueries :: Text -> Context (Token Text) -> Cohort () -> Maybe (Cohort Query)
makeQueries index cxt (Cohort (p,ch))
    | (length . left $ cxt) <= (Pat.size . Pat.left $ p) &&
      (length . right $ cxt) <= (Pat.size . Pat.right $ p) = Just . Cohort . (p,) $
        map (\(t,_) -> (t,Query { queryIndexes = index
                                  , queryComment = T.empty
                                  , queryString = Pat.makeQuery cxt p t})) ch
    | otherwise = Nothing

generateCohortQueries :: Text -> [Cohort ()] -> Context (Token Text) -> [Cohort Query]
generateCohortQueries index cohorts ctx = mapMaybe (makeQueries index ctx) cohorts

type QueryGenerator = Context (Token Text) -> [Cohort Query]

isBaseline :: Prediction -> Bool
isBaseline Baseline { } = True
isBaseline _ = False



predictF :: QueryGenerator -> Logger -> ItemProcessor Score
predictF qg log (target,context) = do
    modify' (\s -> s { totalItems = totalItems s + 1 } )
    let cohorts = qg context
    p <- liftM (fmap (const ())) . E.runEitherT . mapM (\c -> lift (attack c) >>= \r -> if isBaseline r then E.right () else E.left r) $ cohorts
    (success,prediction) <- score p (surface target)
    s <- get
    liftIO $ do
        log . Status . T.intercalate "\t" . map T.pack $
            [ show $ totalItems s
            , if success then "Correct" else "Incorrect"
            , T.unpack $ surface target -- lol.
            , T.unpack prediction
            , show $ correctItems s
            , show $ fromIntegral (correctItems s) / (fromIntegral (totalItems s) :: Double)
            , show $ baselineFallbacks s
            , show $ baselineCorrect s
            , show $ M.foldl' (+) 1 (truePositives s)
            , show $ M.foldl' (+) 1 (falsePositives s) ]
        log . Status . T.pack . show . M.toList $ truePositives s
        log . Status . T.pack . show . M.toList $ falsePositives s

score :: Either Prediction () -> Text -> PFEG Score (Bool,Text)
score (Right _) target = do
    mb <- liftM majorityBaseline ask
    if target == mb
       then modify' (\s -> s { baselineFallbacks = baselineFallbacks s + 1
                        , baselineCorrect   = baselineCorrect s + 1
                        , correctItems      = correctItems s + 1 }) >> return (True,mb)
       else modify' (\s -> s { baselineFallbacks = baselineFallbacks s + 1 }) >> return (False,mb)
score (Left w) target =
    if target == predictedWord w
        then modify' (\s -> s { correctItems = correctItems s + 1
                              , truePositives = M.insertWith (+) (pattern w) 1 $ truePositives s }) 
                    >> return (True,predictedWord w)
        else modify' (\s -> s { falsePositives = M.insertWith (+) (pattern w) 1 $ falsePositives s })
                    >> return (False,predictedWord w)

initialScore :: Score
initialScore = Score { totalItems        = 0
                     , correctItems      = 0
                     , baselineFallbacks = 0
                     , baselineCorrect   = 0
                     , truePositives     = M.empty
                     , falsePositives    = M.empty }

data Score = Score { totalItems        :: !Int
                   , correctItems      :: !Int
                   , baselineFallbacks :: !Int
                   , baselineCorrect   :: !Int -- Don't *need* this, but it makes life easier.
                   , truePositives     :: M.HashMap Pat.MatchPattern Int
                   , falsePositives    :: M.HashMap Pat.MatchPattern Int }

type ItemNumber = Integer

learnF :: QueryGenerator -> QueryChan -> ItemProcessor ItemNumber
learnF qg resLog i = do
    modify' (+1)
    itemNumber <- get
    let cohorts = qg $ snd i
    (preds,time) <- doTimed $ mapM attack cohorts
    liftIO $ writeChan resLog $ QueryData itemNumber i preds time

maybePat :: [(t,Maybe q)] -> [(t,q)]
maybePat ls = [ (t,pat) | (t,Just pat) <- ls]

runQueriesChunked :: Configuration -> [Query] -> IO (Sphinx.Result [QueryResult])
runQueriesChunked conf qs' =
    let qs = chunksOf 32 qs'
    in liftM (f $ Sphinx.Ok []) $ mapM (runQueries conf) qs
    where f (Sphinx.Ok ol) [] = Sphinx.Ok ol
          f (Sphinx.Warning t ol) [] = Sphinx.Warning t ol
          f (Sphinx.Ok ol) (r:rs) = case r of
                                       Sphinx.Ok l -> f (Sphinx.Ok $ ol ++ l) rs
                                       Sphinx.Warning w l -> f (Sphinx.Warning w (ol ++ l)) rs
                                       err -> err
          f (Sphinx.Warning ow ol) (r:rs) = case r of
                                       Sphinx.Ok l -> f (Sphinx.Warning ow $ ol ++ l) rs
                                       Sphinx.Warning w l -> f (Sphinx.Warning (T.unlines [ow,w]) $ ol ++ l) rs
                                       err -> err
          f err _ = err

execSecond :: Monad m => (a -> m b) -> (c,a) -> m (c,b)
execSecond k (c,a) = liftM (c,) (k a)

-- FIXME: remove querify, or make it simpler.
querify :: Maybe Text -> PFEG st (Maybe Query)
querify (Just q) = do index <- liftM sphinxIndex ask
                      return $ Just Query { queryString = q, queryIndexes = index, queryComment = T.empty }
querify Nothing = return Nothing

type QueryChan = Chan QueryData

data QueryData = QueryData
    { qItemNumber :: !Integer
    , qItem       :: !(Item Text)
    , qResults    :: [Prediction]
    , qTime       :: !NominalDiffTime }
    deriving (Show)

bs :: LB.ByteString -> Text
bs = decodeUtf8 . SB.concat . LB.toChunks

getQueryResults :: Sphinx.Result [a] -> ([a],Maybe Text)
getQueryResults result =
    case result of
         (Sphinx.Warning warn a) -> (a, Just $ T.concat ["WARNING: ", warn])
         (Sphinx.Ok a)           -> (a, Nothing)
         (Sphinx.Error code msg) -> ([], Just $ T.concat["ERROR (",T.pack . show $ code,"): ", msg])
         (Sphinx.Retry msg)      -> ([], Just $ T.concat["RETRY: ", msg])

type DocId = Int
type DocMap = M.HashMap DocId (Text,Text)

retrieveSentences :: Connection -> Sphinx.Result [QueryResult] -> IO ([DocId], [Sentence Text])
retrieveSentences conn response = do
    let (results,msg) = getQueryResults response
        docids = unique . concatMap parseResult $ results
    when (isJust msg) $ putStrLn $ "WARNING: " ++ (T.unpack . fromJust $ msg)
    ids'n'sentences <- queryDB conn docids
    return $ unzip ids'n'sentences

learnLogger :: Logger -> QueryChan -> PFEG_ ()
learnLogger log c = do
        session <- ask
        (QueryData itemNumber (target,context) preds time) <- liftIO $ readChan c
        liftIO . log . Status $ T.unwords ["Querying Sphinx took",T.pack . renderS $ time]
        let ctxt = fmap surface context
            isCorrect Baseline { } = corr $ (majorityBaseline session) == surface target
            isCorrect Winner { predictedWord = w } = corr $ w == surface target
            corr False = "Incorrect"
            corr True = "Correct"
            prediction Baseline { } = majorityBaseline session
            prediction Winner { predictedWord = w } = w
            predictionCounts Baseline { } = T.pack $ show (0 :: Int)
            predictionCounts Winner { hits = h } = T.pack $ show h
            line p = [ T.pack $ show itemNumber
                        , T.unwords . left $ ctxt
                        , surface target
                        , T.unwords . right $ ctxt
                        , T.pack . Pat.showShort . pattern $ p
                        , isCorrect p
                        , prediction p
                        , predictionCounts p
                        , T.pack . show . (round :: NominalDiffTime -> Integer) $ time ]
        mapM_ (liftIO . log . Stats . line) $ preds

correctness :: Text -> [((Text,Pat.MatchPattern),Int)] -> [Text]
correctness target ls = let p = cr . head . sortBy (flip compare `on` snd) $ ls
                            cr ((t,_),c) = c /= 0 && t == target
                        in replicate (length ls) $ if p then "Correct" else "Incorrect"

pfegLogger :: Handle -> Maybe Handle -> LogChan -> IO ()
pfegLogger debugH resultH c = do
    msg <- atomically $ readTChan c
    case msg of
        s@(Stats _) -> maybe (return ()) (\h -> hPrint h s >> hFlush h) resultH
        msg' -> hPrint debugH msg' >> hFlush debugH

type LogChan = TChan LogMessage

class RenderLog a where
    renderLog :: a -> Text

data LogMessage = Status Text
                | forall a. RenderLog a => LogItem Text a
                | Warning Text
                | Error Text
                | Stats [Text]

instance RenderLog Pat.MatchData where
    renderLog Pat.MatchData { Pat.predictedTarget = predT
                            , Pat.interferingWords = _intW
                            , Pat.matchPattern = mP } = T.unwords [surface predT,renderLog mP]

instance RenderLog ScoredMatchData where
    renderLog (p,(pat,s)) = T.unwords ["Prediction",p,"—","Pattern",renderLog pat,"Score",renderLog s]

instance RenderLog Double where
    renderLog = T.pack . show

instance (RenderLog a) => RenderLog (Maybe a) where
    renderLog Nothing = "nothing"
    renderLog (Just a) = renderLog a

instance RenderLog (Pat.MatchPattern,Query) where
    renderLog (a,b) = T.unwords [renderLog a,renderLog b]

instance Show LogMessage where
    show (Status t)    = "INFO: " ++ T.unpack t
    show (Warning t)   = "WARN: " ++ T.unpack t
    show (Error t)     = "ERR:  " ++ T.unpack t
    show (LogItem t i) = "*** " ++ T.unpack t ++":\n" ++ (T.unpack . renderLog $ i) ++  "\n***"
    show (Stats s)     = T.unpack . T.intercalate "\t" $ s

instance RenderLog Text where
    renderLog = id

instance RenderLog (DocId,Sentence Text) where
    renderLog (i,s) = T.concat [renderLog i, ": ", showSentence s]

instance RenderLog Int where
    renderLog = T.pack . show

instance RenderLog (Sphinx.Result [QueryResult]) where
    renderLog (Sphinx.Ok r) = renderLog r
    renderLog (Sphinx.Warning w r) = T.unlines [T.concat ["SPHINX WARNING: ",T.pack (show w)],renderLog r]
    renderLog (Sphinx.Error _ w) = T.concat ["SPHINX ERROR: ", T.pack (show w)]
    renderLog (Sphinx.Retry w) = T.concat ["SPHINX RETRY: ", T.pack (show w)]

instance RenderLog QueryResult where
    renderLog qr = T.pack . groom $ qr

instance RenderLog (Item Text) where
    renderLog (t,Context l r) = T.unlines
                   [ T.unwords ["Left context:",wrap '\'' (T.unwords (map surface l))]
                   , T.unwords ["Target:",surface t]
                   , T.unwords ["Right context:",wrap '\'' (T.unwords (map surface r))]]

instance RenderLog (Context (Token Text)) where
    renderLog (Context l r) =
         T.unlines [ T.unwords ["Left context:",wrap '\'' (T.unwords (map surface l))]
                   , T.unwords ["Right context:",wrap '\'' (T.unwords (map surface r))]]

instance RenderLog Pat.MatchPattern where
    renderLog = T.pack . show

showSentence :: Sentence Text -> Text
showSentence = T.unwords . map surface

instance (RenderLog a) => RenderLog [a] where
    renderLog xs = wrap2 '[' ']' . T.intercalate "\n," . map renderLog $ xs

instance RenderLog Query where
    renderLog = queryString

showScoredData :: Item Text -> Int -> ScoredMatchData -> String
showScoredData (t',ctxt) i (t,(p,s)) = intercalate "\t"
    [ show i                                -- Index
    , T.unpack . surface $ t'               -- Actual target
    , T.unpack t                            -- Predicted target
    , show p                                -- Pattern
    , show s                                -- Score
    , T.unpack . T.unwords . map surface . left $ ctxt    -- Left context
    , T.unpack . T.unwords . map surface . right $ ctxt ] -- Right context

getMatches :: [Text] -> [Pat.MatchPattern] -> Context (Token Text) -> Sentence Text -> [Pat.MatchData]
getMatches trgs pats c s = concatMap (\p -> Pat.matchParser p trgs c s) pats

type ScoredMatchData = (Text,(Pat.MatchPattern,Double))

findBestPrediction :: Context (Token Text) -> [ScoredMatchData] -> Maybe ScoredMatchData
findBestPrediction _ =
    listToMaybe . sortBy (flip compare `on` (snd . snd)) . M.toList . M.fromListWith addWeights
    where addWeights (p,x) (_,y) = (p, x + y)

scoreMatchData :: [Pat.MatchData] -> [ScoredMatchData]
scoreMatchData = map f
    where f d = ( surface . Pat.predictedTarget $ d
                , ( Pat.matchPattern d
                  , computeIntWeight (Pat.interferingWords d) + (Pat.weight . Pat.matchPattern $ d)))

-- | TODO: dummy
computeIntWeight :: Pat.Interference (Token Text) -> Double
computeIntWeight = const 0.0

-- | Remove duplicates from a list
unique :: (Hashable a, Eq a) => [a] -> [a]
unique = Set.toList . Set.fromList

-- | Retrieve excerpts from the Sphinx server.
getEx :: ExcerptConfiguration -> String -> [Text] -> Text -> IO ([Text], Maybe Text)
getEx ec index docs qry = liftM getQueryResults $ buildExcerpts ec docs (T.pack index) qry

-- | Count elements in a list
count :: (Hashable a, Eq a, Num b, Ord b) => [a] -> [(a,b)]
count x = sortBy (compare `on` snd) . M.toList . M.fromListWith (+) $ zip x (repeat 1)

-- | Find document text by DocId in a DocMap
lookupDoc :: DocMap -> DocId -> (Text,Text)
lookupDoc = flip (M.lookupDefault (T.empty,T.empty))

-- | Retrieve all matches of group 1 of the regex in the string.
getPrediction :: Regex -> Text -> [Text]
getPrediction rex exc = mapMaybe (group 1) . findAll rex $ exc

-- | Select either surface or lemma depending on the query string.
--   Expects the documents to be of form (surface,lemma), and will choose
--   surface by default.
chooseSide :: Text          -- ^ The query string
             -> [(Text,Text)] -- ^ Documents with surface and lemma
             -> [Text]        -- ^ Either surface or lemma, depending on the query
chooseSide q d | "@lemma" `T.isPrefixOf` q = map snd d
               | otherwise                 = map fst d

-- | Get sentence data structures of documents from the DB in batch.
queryDB :: Connection -> [DocId] -> IO [(DocId,Sentence Text)]
queryDB _ [] = return []
queryDB conn ids = do
    let arg = (++")") . ('(':) . intercalate "," . map show $ ids
    sql <- liftIO $ quickQuery' conn ("SELECT id,pos,lemma,surface FROM records WHERE id in " ++ arg) []
    return $ map mkSentence sql
    where mkSentence :: [SqlValue] -> (DocId,Sentence Text)
          -- The reason we're using the explicit Word record syntax here is to never mix up surface, pos, and lemma
          -- as this can lead to embarrassing and difficult to detect bugs.
          mkSentence (i:s:l:p:[]) = (fromSql i,zipWith3 (\s' l' p' -> Word { pos = p', lemma = l', surface = s' })
                                    (sqlw p) (sqlw l) (sqlw s))
          mkSentence x          = error $ "SQL reply not in the right format:\n" ++ groom x
          sqlw :: SqlValue -> [Text]
          sqlw = T.words . fromSql

untext :: [Text] -> String
untext = T.unpack . T.unwords

next, correct :: MonadState MatcherState m => m ()
next    = modify' $ \ (MatcherState t c) -> MatcherState (t+1) c
correct = modify' $ \ (MatcherState t c) -> MatcherState t (c+1)

data MatcherState = MatcherState { totalMatches :: !Int, correctMatches :: !Int }

parseResult :: QueryResult -> [DocId]
parseResult QueryResult { matches = ms } = map (fromIntegral . Sphinx.documentId) ms

initialMatcherState :: MatcherState
initialMatcherState = MatcherState 0 0

correctPrediction :: (MonadState MatcherState m) => m ()
correctPrediction = modify' (\MatcherState { totalMatches = t, correctMatches = c } ->
                              MatcherState (t+1) (c+1))

data SphinxPattern = Surface { width :: !Int, tolerance :: !Int }
                   | Lemma   { width :: !Int, tolerance :: !Int }
                   | MajorityBaseline

instance Show SphinxPattern where
    show MajorityBaseline = "majority baseline"
    show p = getLetter p : show (width p) ++ show (tolerance p)

getLetter :: SphinxPattern -> Char
getLetter Surface {} = 's'
getLetter Lemma   {} = 'l'
getLetter MajorityBaseline = 'M'

wrap :: Char -> Text -> Text
wrap c = wrap2 c c

wrap2 :: Char -> Char -> Text -> Text
wrap2 a b t = T.cons a $ T.concat [t, T.singleton b]

workOnCorpora :: Nullable a => Text -> TBChan DBAction -> I.Iteratee Text (PFEG st) a -> I.Iteratee a (PFEG st) () -> PFEGConfig -> st -> [Corpus] -> IO ()
workOnCorpora action tchan it1 it2 session st = mapM_ $ \ c@(cName,cFile) -> do
    (threadID,logVar) <- evalPFEG (forkLogger c) () session
    timeStarted <- getCurrentTime
    let iteratee = I.run =<< enumFile (chunkSize session) cFile (I.sequence_
                   [ countChunksI logVar
                   , I.mapChunks (toUnicode (corpusConverter session)) I.><> I.convStream it1 I.=$ it2])
    -- log to db that we're starting here
    atomically $ writeTBChan tchan (Log (T.unwords [action,"start"]) c)
    _ <- execPFEG iteratee st session
    killThread threadID
    timeFinished <- getCurrentTime
    putStrLn $ "Finished " ++ cName ++ " in " ++ renderS (timeFinished `diffUTCTime` timeStarted)
    -- log to db that we're finished here and commit
    atomically $ do writeTBChan tchan (Log (T.unwords [action,"end"]) c)
                    writeTBChan tchan Commit

recorder :: Statement -> MVar RecordData -> MVar () -> IO ()
recorder s mvar cmd = do
    takeMVar cmd
    vals <- takeMVar mvar
    void $ executeMany s vals
    putMVar cmd ()

type RecordData = [[SqlValue]]

recordDocsF :: TBChan DBAction -> Statement -> DocumentProcessor_
recordDocsF chan stmt doc = liftIO $ atomically (writeTBChan chan (DBWrite stmt [vals]))
    where vals = document2SQL doc

recordF :: TBChan DBAction -> Statement -> SentenceProcessor (Int, RecordData)
recordF chan stmt s = do
    (i,vals) <- get
    let vals' = sentence2SQL s:vals
    if i == 7000
       then put (0,[]) >> liftIO (atomically (writeTBChan chan (DBWrite stmt vals')))
       else put (i+1,vals')

itemIteratee :: ItemGetter -> ItemProcessor st -> Iteratee (Document Text) (PFEG st) ()
itemIteratee gI proc = I.mapChunksM_ $ mapM proc . gI

sentenceIteratee :: SentenceProcessor st -> Iteratee (Sentence Text) (PFEG st) ()
sentenceIteratee = I.mapChunksM_

documentIteratee :: DocumentProcessor st -> Iteratee (Document Text) (PFEG st) ()
documentIteratee = I.mapChunksM_

type DocumentProcessor st = Document Text -> PFEG st ()
type DocumentProcessor_ = DocumentProcessor ()

type SentenceProcessor st = Sentence Text -> PFEG st ()
type SentenceProcessor_   = SentenceProcessor ()

type ItemProcessor st = Item Text -> PFEG st ()
type ItemProcessor_ = ItemProcessor ()

forkLogger :: Corpus -> PFEG () (ThreadId,Chan Int)
forkLogger (cName,cFile) = do
    session <- ask
    liftIO $ do
        logVar <- newChan
        csize  <- withFile cFile ReadMode hFileSize
        putStrLn $ "Processing '" ++ cName ++ "' at '" ++ cFile ++ ".'"
        threadID <- forkIO $ logger (fromIntegral csize `div` chunkSize session) logVar
        return (threadID,logVar)

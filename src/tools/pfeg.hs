{-# LANGUAGE TupleSections, FlexibleInstances, TypeSynonymInstances, ExistentialQuantification, OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}
module Main where

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
import qualified Data.List as L

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
          let it = itemIteratee (getSentenceItems (`elem` targets session)) (learnF chan)
          workOnCorpora "match" tchan (documentI res) it session 0 cs
        Predict { corpora = cs } -> do
          chan <- newChan
          -- TODO: fork a score thread
          let it = itemIteratee (getSentenceItems (`elem` targets session)) (predictF chan)
          workOnCorpora "predict" tchan (documentI res) it session () cs
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

predictF :: QueryChan -> ItemProcessor_
predictF = undefined

type ItemNumber = Integer

-- There's some pretty unholy zipping and unzipping and tupling and
-- untupling going on here, which *could* be a performance hog. Or not.
-- Maybe we could factor that out into a separate thread.
learnF :: QueryChan -> ItemProcessor ItemNumber
learnF resLog i = do
    session <- ask
    modify' (+1)
    itemNumber <- get
    (ts,queries) <- liftM (unzip . maybePat) . mapM (execSecond querify) $ [ ((targ,pat),Pat.makeQuery (snd i) pat targ) 
                                                 | pat <- matchPatterns session
                                                 , targ <- targets session ]
    liftIO $ do
        (results',time) <- doTimed $ runQueriesChunked (searchConf.pfegMode $ session) queries
        let (results,errmsg) = getQueryResults results'
        case errmsg of
            Just err -> putStrLn $ "FAILED TO GET RESULTS: " ++ T.unpack err
            Nothing -> writeChan resLog $ QueryData itemNumber i (queries,ts,results) time

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
    , qResults    :: ([Query],[(Text,Pat.MatchPattern)],[QueryResult])
    , qTime       :: !NominalDiffTime }
    deriving (Show)

initialPredictScore, initialMatchScore :: Score
initialPredictScore = PredictScore 0 0 0 0 0 0 0 0 0 0
initialMatchScore = MatchScore 0 0

tickScore :: (MonadState Score m) => m ()
tickScore = modify' $ \ s -> s { totalScored = totalScored s+1 }

score :: Maybe Text -> Item Text -> PFEG Score Score
score p i = do
    mB <- liftM majorityBaseline ask
    let isCorrect = case p of
                         Nothing -> mB == surface (fst i)
                         Just x  -> x == surface (fst i)
    oldScore <- get
    if isCorrect
       then return $ oldScore { scoreCorrect = scoreCorrect oldScore + 1, totalScored = totalScored oldScore + 1 }
       else return $ oldScore { totalScored = totalScored oldScore + 1 }

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
learnLogger log c = liftIO $ do
    (QueryData itemNumber item (queries,ts,results') time) <- readChan c
    log . Status $ T.unwords ["Querying Sphinx took",T.pack . renderS $ time]
    let results = map total results'
        preds = concatMap (correctness $ surface . fst $ item) $ L.groupBy (\((_,a),_) ((_,b),_) -> a == b) $ zip ts results
    mapM_ (logLine itemNumber time item) $ L.zip4 queries ts results preds
    where  logLine itemNumber time (target,context) (q,(predic,patt),res,correctPred) =
             let ctxt = fmap surface context
             in  log $ Stats [ T.pack $ show itemNumber
                             , T.unwords . left $ ctxt
                             , surface target
                             , T.unwords . right $ ctxt
                             , queryString q
                             , T.pack . Pat.showShort $ patt
                             , predic
                             , T.pack . show $ res
                             , T.pack . show . (round :: NominalDiffTime -> Integer) $ time
                             , correctPred ]

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

scoreboard :: Score -> [String]
scoreboard s@MatchScore { }   = map show (zipWith ($) [totalScored, scoreCorrect] (repeat s))
scoreboard s@PredictScore { } = map show (zipWith ($) 
                                          ([totalScored, scoreCorrect] ++ canonicalPredictScore)
                                          (repeat s))

canonicalPredictScore :: [Score -> Int]
canonicalPredictScore = [ scoreAAA
                        , scoreAAB
                        , scoreABA
                        , scoreABB
                        , scoreABC
                        , scoreAAAContained
                        , scoreAABContained
                        , scoreABBContained ]

-- first three predictions
predictions :: Prediction -> [String]
predictions ps = take 6 $ concatMap (\ (a,b) -> [T.unpack a,show b] ) ps ++ repeat ""

untext :: [Text] -> String
untext = T.unpack . T.unwords

next, correct :: MonadState MatcherState m => m ()
next    = modify' $ \ (MatcherState t c) -> MatcherState (t+1) c
correct = modify' $ \ (MatcherState t c) -> MatcherState t (c+1)

data Score = PredictScore { totalScored       :: !Int
                          , scoreAAA          :: !Int
                          , scoreAAB          :: !Int
                          , scoreABA          :: !Int
                          , scoreABB          :: !Int
                          , scoreABC          :: !Int
                          , scoreAAAContained :: !Int
                          , scoreAABContained :: !Int
                          , scoreABBContained :: !Int
                          , scoreCorrect :: !Int }
           | MatchScore   { totalScored  :: !Int
                          , scoreCorrect :: !Int }

instance Show Score where
    show s = '(':show (scoreCorrect s) ++ "/" ++ show (totalScored s) ++ ") " ++
             case s of
                  MatchScore { } -> ""
                  PredictScore { } -> intercalate ", " $ zipWith f 
                                      ["aaa","aab","aba","abb","abc","aaaC","aabC","abbC"]
                                      canonicalPredictScore
                        where f name scr = name ++ ": " ++ show (scr s)

type Prediction = [(Text,Int)]
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

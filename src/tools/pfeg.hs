{-# LANGUAGE RankNTypes, BangPatterns, DeriveFunctor, TupleSections, FlexibleInstances, ExistentialQuantification, OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-} -- holy language extensions batman.
module Main where

import Control.Concurrent (ThreadId,killThread,forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan

import Control.Exception (bracket,throw)
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Control.Monad.Trans.Either as E

import Data.Iteratee.Base
import Data.Iteratee.IO
import qualified Data.Iteratee as I

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Text.ICU.Convert
import Data.Time.Clock (diffUTCTime,getCurrentTime,NominalDiffTime)
import Data.Time.LocalTime
import Database.HDBC
import Database.HDBC.MySQL
import Graphics.Vty.Terminal
import System.IO
import Text.Groom
import qualified ReadArgs as RA
import qualified Data.List as L
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Random as R
import qualified Data.Random.Extras as RE

import PFEG
import PFEG.Common
import PFEG.Configuration
import PFEG.Context
import PFEG.SQL
import PFEG.Types
import qualified PFEG.Pattern as Pat

import Text.Search.Sphinx hiding (sortBy,mode)
import Text.Search.Sphinx.Types (QueryResult(..))
import qualified Text.Search.Sphinx.Types as Sphinx

import Prelude hiding (log)

-- All actions the db is capable of performing are encoded into this
-- datatype, which is sent over a channel to "dbthread" which performs
-- them.
data DBAction = DBWrite Statement [[SqlValue]] -- ^ Write a bunch of SQL to the db using "executeMany"
              | Log Text Corpus -- ^ Log an action into the log table
              | Commit -- ^ Commit (finish the transaction)
              | Shutdown -- ^ Kill yourself
              | ShutdownACK -- ^ Yes, I've killed myself.

-- This thread receives DBAction instructions over a bounded channel.
-- It is pfeg's main way to interact with the database.
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

-- Control passes through here after initialization. It sets up all the DB
-- and logging threads that are necessary, plus the required communication
-- channels. It then passes control (and all necessary preconfigured actions)
-- to "workOnCorpora" which is going to do the actual work for all corpus
-- files. After workOnCorpora is finished, it waits for the DB to be ready.
process :: PFEGConfig -> IO ()
process session = do
    tchan <- atomically $ newTBChan 2
    _dbthread_id <- forkIO $ dbthread (database session) tchan
    logChan <- atomically newTChan
    let getStatsLog PFEGConfig { pfegMode = Learn { statLog = rl } } = Just rl
        getStatsLog PFEGConfig { pfegMode = Predict { predictLog = pl } } = Just pl
        getStatsLog _ = Nothing
    _debLthread_id <- forkIO . forever $ pfegLogger (debugLog session) (getStatsLog session) logChan
    let log = atomically . writeTChan logChan
        res = cardRegexes session
        (_,sampleSize) = sample session
        qg = generateCohortQueries (sphinxIndex session) $ makeCohorts (targets session) (matchPatterns session)
    case pfegMode session of
        Record{ corpora = cs } -> do
          s <- prepare (database session) insertText
          let it = documentIteratee (recordDocsF tchan s)
          workOnCorpora "record" tchan (documentI res)it session () cs
        Learn{ corpus = cs } -> do
          chan <- newChan
          void $ forkIO (evalPFEG (forever $ learnLogger log chan) () session)
          let ig = getDocumentItems (`elem` targets session)
          randomSample <- extractRandomSample ig () (documentI res) session cs
          (threadID,logVar) <- evalPFEG (statusUpdater' "to process sample" sampleSize) () session
          void $ evalPFEG (mapM (learnF logVar qg chan) randomSample) 0 session
          killThread threadID
        Predict { corpus = cs } -> do
          let ig = getUniqueDocumentItems (`elem` targets session)
          randomSample <- extractRandomSample ig () (documentI res) session cs
          (threadID,logVar) <- evalPFEG (statusUpdater' "to process sample" sampleSize) () session
          void $ evalPFEG (mapM (predictF logVar qg log) randomSample) initialScore session
          killThread threadID
    putStrLn "Waiting for DB…"
    atomically $ writeTBChan tchan Shutdown
    atomically $ do
        r <- tryPeekTBChan tchan
        unless (dbthreadIsDone r) retry
        where dbthreadIsDone (Just ShutdownACK) = True
              dbthreadIsDone _ = False

-- Logging action to be passed to the actual worker threads so they can log
-- stuff to the debug or stats log.
type Logger = LogMessage -> IO ()

-- A cohort is a bunch of similar queries which differ only in the target
-- preposition. This type records the target outside of the query, and the
-- "Pat.MatchPattern" for convenience.
newtype Cohort a = Cohort { extractCohort :: (Pat.MatchPattern,[(Text,a)]) }
                   deriving (Functor,Show)

-- | From a list of targets and patterns, generate all possible cohorts,
-- sorted by pattern weight.
makeCohorts :: [Text] -> [Pat.MatchPattern] -> [Cohort ()]
makeCohorts ts = map (\p -> Cohort . (p,) $ zip ts (repeat ())) . sortBy (flip compare `on` Pat.weight)

-- A prediction is either a baseline prediction (no evidence found in
-- corpus) or a winner with an associated count, pattern, and target.
data Prediction = Baseline { pattern :: !Pat.MatchPattern }
                | Winner { predictedWord :: !Text, hits :: !Int, pattern :: !Pat.MatchPattern } deriving Show

-- Given a Cohort returned from the search server, turn it into
-- a prediction.
winner :: Cohort Int -> Prediction
winner (Cohort (pat,[])) = Baseline pat
winner (Cohort (pat,counts)) = case head . sortBy (flip compare `on` snd) $ counts of
                                 (_,0) -> Baseline pat
                                 (t,c) -> Winner t c pat

-- THIIIISS IIIIS… really slow.
-- Also, it does the job of taking a cohort and querying sphinx for it.
attack :: Cohort Query -> PFEG st Prediction
attack (Cohort (p,qs)) = do
    session <- ask
    (counts,warning) <- liftIO $ liftM getCounts $ runQueries (searchConf session) (map snd qs)
    unless (T.null warning) . liftIO . putStrLn $ ("WARNING: " ++ T.unpack warning)
    return $ winner $ Cohort (p,zipWith (\ (t,_) r -> (t,r)) qs counts)

-- From a query result, extract the hit counts, if possible.
getCounts :: Sphinx.Result [QueryResult] -> ([Int],Text)
getCounts (Sphinx.Ok a)        = (map total a,T.empty)
getCounts (Sphinx.Warning t a) = (map total a,t)
getCounts (Sphinx.Error err t) = ([],T.intercalate ": " [T.pack (show err),t])
getCounts (Sphinx.Retry t)     = ([],t)

-- Given an index, a certain context and an unitialized cohort, initialize
-- the cohort to the query, but only in case the cohort's pattern isn't too
-- big for the context.
makeQueries :: Text -> Context (Token Text) -> Cohort () -> Maybe (Cohort Query)
makeQueries index cxt (Cohort (p,ch))
    | (length . left $ cxt) >= (Pat.size . leftWindow $ p) &&
      (length . right $ cxt) >= (Pat.size . rightWindow $ p) = Just . Cohort . (p,) $
        map (\(t,_) -> (t,Query { queryIndexes = index
                                  , queryComment = T.empty
                                  , queryString = Pat.makeQuery cxt p t})) ch
    | otherwise = Nothing

-- Given an index, all possible cohorts for this task and a context, make
-- all possible query cohorts for this contexts (see "makeQueries")
generateCohortQueries :: Text -> [Cohort ()] -> Context (Token Text) -> [Cohort Query]
generateCohortQueries index cohorts ctx = mapMaybe (makeQueries index ctx) cohorts

-- Partially saturated generateCohortQueries (since, typically, index and
-- cohorts don't change.)
type QueryGenerator = Context (Token Text) -> [Cohort Query]

-- Return @True@ iff a prediction is "Baseline"
isBaseline :: Prediction -> Bool
isBaseline Baseline { } = True
isBaseline _ = False

-- The predict-worker. Keeps track of the "Score", and for each item it
-- gets, it queries the search server for all cohorts, sorted by their
-- pattern weight (starting with the highest) until it finds a cohort that
-- can predict something that is not "Baseline". It then compares this to
-- the item's target, and updates the score accordingly. If it can't find
-- a non-baseline prediction, it assumes baseline, matches against the
-- item's target, and updates the score accordingly, but modifying the
-- baseline counts instead of the pattern counts.
predictF :: Chan Int -> QueryGenerator -> Logger -> ItemProcessor Score
predictF statusChan qg log (target,context) = do
    session <- ask
    modify' (\s -> s { totalItems = totalItems s + 1 } )
    let cohorts = qg context
    p <- liftM void . E.runEitherT . mapM (\c -> lift (attack c) >>= \r -> if isBaseline r then E.right () else E.left r) $ cohorts
    (success,prediction) <- score p (surface target)
    s <- get
    liftIO $ do
        log . Status . T.intercalate "\t" . map T.pack $
            [ show $ totalItems s
            , if success then "Correct" else "Incorrect"
            , T.unpack $ if isBaseline prediction then majorityBaseline session else predictedWord prediction
            , T.unpack $ surface target
            , show $ correctItems s
            , show $ fromIntegral (correctItems s) / (fromIntegral (totalItems s) :: Double)
            , show $ baselineFallbacks s
            , show $ baselineCorrect s
            , show $ M.foldl' (+) 1 (truePositives s)
            , show $ M.foldl' (+) 1 (falsePositives s) ]
        log . Status . T.pack . show . M.toList $ truePositives s
        log . Status . T.pack . show . M.toList $ falsePositives s
        log . Stats $ [ T.pack . show . totalItems $ s
                      , if isBaseline prediction then majorityBaseline session else predictedWord prediction
                      , T.unwords . map surface . left $ context
                      , surface target
                      , T.unwords . map surface . right $ context
                      , if isBaseline prediction then "Baseline" else T.pack . show . pattern $ prediction
                      , if isBaseline prediction then T.singleton '0' else T.pack . show . hits $ prediction
                      , if success then "Correct" else "Incorrect" ]
        writeChan statusChan $ totalItems s

-- @Either Prediction ()@ means: either @Left@ contains an actual
-- prediction (no "Baseline"), or @Right@ contains nothing, which means we
-- have to predict baseline. This then updates the "Score" in "PFEG"
-- accordingly, and returns whether the prediction was successful, and also
-- the prediction as Text.
score :: Either Prediction () -> Text -> PFEG Score (Bool,Prediction)
score (Right _) target = do
    mb <- liftM majorityBaseline ask
    if target == mb
       then modify' (\s -> s { baselineFallbacks = baselineFallbacks s + 1
                        , baselineCorrect   = baselineCorrect s + 1
                        , correctItems      = correctItems s + 1 }) >> return (True,Baseline NoPattern)
       else modify' (\s -> s { baselineFallbacks = baselineFallbacks s + 1 }) >> return (False,Baseline NoPattern)
score (Left w) target =
    if target == predictedWord w
        then modify' (\s -> s { correctItems = correctItems s + 1
                              , truePositives = M.insertWith (+) (pattern w) 1 $ truePositives s }) 
                    >> return (True,w)
        else modify' (\s -> s { falsePositives = M.insertWith (+) (pattern w) 1 $ falsePositives s })
                    >> return (False,w)

-- Initial "Score" object, with everything set to zero.
initialScore :: Score
initialScore = Score { totalItems        = 0
                     , correctItems      = 0
                     , baselineFallbacks = 0
                     , baselineCorrect   = 0
                     , truePositives     = M.empty
                     , falsePositives    = M.empty }

-- Score data sctructure which holds the predictor's scores.
data Score = Score { totalItems        :: !Int -- ^ How many items have we seen?
                   , correctItems      :: !Int -- ^ How many were predicted correctly
                   , baselineFallbacks :: !Int -- ^ How often did we have to fall back to baseline?
                   , baselineCorrect   :: !Int -- Don't *need* this, but it makes life easier.
                   , truePositives     :: M.HashMap Pat.MatchPattern Int -- True positives per pattern
                   , falsePositives    :: M.HashMap Pat.MatchPattern Int -- False positives per pattern
                   }

type ItemNumber = Integer

-- The learn-worker. Execute all possible cohort queries and log the
-- results to an R-friendly CSV format. The logging is actually done by
-- "learnLogger" and sent to it via a "QueryChan" that cointains
-- "QueryData".
learnF :: Chan Int -> QueryGenerator -> QueryChan -> ItemProcessor ItemNumber
learnF statusChan qg resLog i = do
    modify' (+1)
    itemNumber <- get
    let cohorts = qg $ snd i
    (preds,time) <- doTimed $ mapM attack cohorts
    liftIO $ do writeChan resLog $ QueryData itemNumber i preds time
                writeChan statusChan $ fromIntegral itemNumber

type QueryChan = Chan QueryData

data QueryData = QueryData
    { qItemNumber :: !Integer
    , qItem       :: !(Item Text)
    , qResults    :: [Prediction]
    , qTime       :: !NominalDiffTime }
    deriving (Show)

-- Do the actual logging of the "learnF" action. This is responsible for
-- flushing the results out to the csv file.
learnLogger :: Logger -> QueryChan -> PFEG_ ()
learnLogger log c = do
        session <- ask
        (QueryData itemNumber (target,context) preds time) <- liftIO $ readChan c
        liftIO . log . Status $ T.unwords ["Querying Sphinx took",T.pack . renderS $ time]
        let ctxt = fmap surface context
            corr False = "Incorrect"
            corr True = "Correct"
            line p = [ T.pack $ show itemNumber
                     , T.unwords . left $ ctxt
                     , surface target
                     , T.unwords . right $ ctxt ] ++ patternStats p ++ 
                     [ T.pack . show . (round :: NominalDiffTime -> Integer) $ time ]
            patternStats Baseline { pattern = pat } = 
                                    [ T.pack . Pat.showShort $ pat
                                    , corr $ majorityBaseline session == surface target
                                    , majorityBaseline session
                                    , T.singleton '0' ]
            patternStats Winner   { pattern = pat, predictedWord = w, hits = predictionCounts } =
                                    [ T.pack . Pat.showShort $ pat
                                    , corr $ w == surface target
                                    , w
                                    , T.pack . show $ predictionCounts ]
        mapM_ (liftIO . log . Stats . line) preds

-- This is the debug logger.
pfegLogger :: Handle -> Maybe Handle -> LogChan -> IO ()
pfegLogger debugH resultH c = do
    msg <- atomically $ readTChan c
    case msg of
        s@(Stats _) -> maybe (return ()) (\h -> hPrint h s >> hFlush h) resultH
        msg' -> hPrint debugH msg' >> hFlush debugH

type LogChan = TChan LogMessage

-- Stuff that can be logged.
class RenderLog a where
    renderLog :: a -> Text

-- The different log messages that we can render.
data LogMessage = Status Text
                | forall a. RenderLog a => LogItem Text a
                | Warning Text
                | Error Text
                | Stats [Text]

instance (RenderLog a) => RenderLog (Maybe a) where
    renderLog Nothing = "nothing"
    renderLog (Just a) = renderLog a

instance Show LogMessage where
    show (Status t)    = "INFO: " ++ T.unpack t
    show (Warning t)   = "WARN: " ++ T.unpack t
    show (Error t)     = "ERR:  " ++ T.unpack t
    show (LogItem t i) = "*** " ++ T.unpack t ++":\n" ++ (T.unpack . renderLog $ i) ++  "\n***"
    show (Stats s)     = T.unpack . T.intercalate "\t" $ s

instance RenderLog Text where
    renderLog = id

instance (RenderLog a) => RenderLog [a] where
    renderLog xs = wrap2 '[' ']' . T.intercalate "\n," . map renderLog $ xs

instance RenderLog Query where
    renderLog = queryString

-- This is the most complicated part. For every corpus file, it first forks
-- a status updater. It then constructs an Iteratee that will a)
-- update the status logger, b) process the corpus file. At the end of each
-- corpus file, it sends the "Commit" action to the DB.
workOnCorpora :: Nullable a => Text -- ^ The name of the action we're performing (for logging)
                 -> TBChan DBAction -- ^ The database channel
                 -> I.Iteratee Text (PFEG st) a -- ^ Parsing iteratee
                 -> I.Iteratee a (PFEG st) () -- ^ Sink iteratee
                 -> PFEGConfig -- ^ configuration
                 -> st -- ^ initial state for the iteratees
                 -> [Corpus] -- ^ List of corpus files
                 -> IO ()
workOnCorpora action tchan it1 it2 session st = mapM_ $ \ c@(cName,cFile) -> do
    (threadID,logVar) <- evalPFEG (statusUpdater c) () session
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

-- Same as workOnCorpora but samples its corpus instead. It then returns
-- a randomized list of items, that can then be further processed.
extractRandomSample :: ItemGetter -> st -> I.Iteratee Text (PFEG st) (Document Text) -> PFEGConfig -> Corpus -> IO [Item Text]
extractRandomSample ig st it1 session (cName,cFile) = do
    let (sampleFrom,sampleSize) = sample session
    (threadID,logVar) <- evalPFEG (statusUpdater' "sampling process" sampleSize) () session
    putStrLn $ "Sampling " ++ show sampleSize ++ " items from " ++ show sampleFrom
                ++ " items in '" ++ cName ++ "' at '" ++ cFile ++ ".'"
    spl <- R.runRVar (RE.sample sampleSize [1..sampleFrom]) R.StdRandom
    putStrLn "Acquiring sample."
    let sampleit = I.mapChunks (toUnicode (corpusConverter session)) -- convert BS to unicode Text
                   I.><> I.convStream it1 -- parse the Text into Document Text
                   I.><> I.mapChunksM ig -- Turn Documents into [Item Text]
                   I.=$ selectSampleI logVar spl -- select a sample of all [Item Text]s
    items <- evalPFEG (I.run =<< enumFile (chunkSize session) cFile sampleit) st session
    killThread threadID
    putStrLn "Randomizing sample."
    R.runRVar (R.shuffle items) R.StdRandom

-- | The data that will be written to the dbthread.
type RecordData = [[SqlValue]]

-- Write documents to the database via the dbthread.
recordDocsF :: TBChan DBAction -> Statement -> DocumentProcessor_
recordDocsF chan stmt doc = liftIO $ atomically (writeTBChan chan (DBWrite stmt [vals]))
    where vals = document2SQL doc

-- Unused, would do the same, but with sentences, making one document per
-- sentence, not one document per WAC-@<text>@ .
recordF :: TBChan DBAction -> Statement -> SentenceProcessor (Int, RecordData)
recordF chan stmt s = do
    (i,vals) <- get
    let vals' = sentence2SQL s:vals
    if i == 7000
       then put (0,[]) >> liftIO (atomically (writeTBChan chan (DBWrite stmt vals')))
       else put (i+1,vals')

-- Apply the processor for every item (and get items with the supplied
-- "ItemGetter".
itemIteratee :: ItemGetter -> ItemProcessor st -> Iteratee (Document Text) (PFEG st) ()
itemIteratee gI proc = I.mapChunksM_ (gI >=> mapM_ proc)

-- Apply the processor for every sentence
sentenceIteratee :: SentenceProcessor st -> Iteratee (Sentence Text) (PFEG st) ()
sentenceIteratee = I.mapChunksM_

-- Apply the processor for every document
documentIteratee :: DocumentProcessor st -> Iteratee (Document Text) (PFEG st) ()
documentIteratee = I.mapChunksM_

type DocumentProcessor st = Document Text -> PFEG st ()
type DocumentProcessor_ = DocumentProcessor ()

type SentenceProcessor st = Sentence Text -> PFEG st ()
type SentenceProcessor_   = SentenceProcessor ()

type ItemProcessor st = Item Text -> PFEG st ()
type ItemProcessor_ = ItemProcessor ()

-- Given a list of indices, select only those indices from an incoming
-- chunked list. Progress is logged to the provided "Chan".
selectSampleI :: Chan Int -> [Int] -> I.Iteratee [a] (PFEG st) [a]
selectSampleI logVar sampleIndexes = I.liftI $ go (kcps id 0 1 (L.sort sampleIndexes))
    where go k (Chunk a) = k a
          go _ _         = throw EofException
          kcps z !c !i js@(j:js') (s:ss) | j == i = let c' = c+1
                                                    in liftIO (writeChan logVar c) 
                                                    >> kcps (z.(s:)) c' (i+1) js' ss
                                         | otherwise = kcps  z       c  (i+1) js  ss
          kcps z _c _i [] _s = I.idone (z []) (EOF Nothing) -- we've exhausted the sample list. done
          kcps z !c !i js [] = I.liftI $ go (kcps z c i js) -- we've exhausted the item stream. continue
          -- where z is the accumulating list continuation, c is a counter
          -- of how many items we've collected (for logging), i is the
          -- current item's index, js are the item numbers we're looking
          -- for in the sample, and s is the stream of items. CPS!

-- This displays progress indicators for corpora.
statusUpdater :: Corpus -> PFEG () (ThreadId,Chan Int)
statusUpdater (cName,cFile) = do
    session <- ask
    csize <- liftIO $ withFile cFile ReadMode hFileSize
    statusUpdater' ("to process '" ++ cName ++ "' at '" ++ cFile ++ ".'")
                   (fromIntegral csize `div` chunkSize session)

-- This is a generic progress indicator for anything numeric.
statusUpdater' :: String -> Int -> PFEG () (ThreadId, Chan Int)
statusUpdater' task maxI = liftIO $ do
    logVar <- newChan
    putStrLn $ "Starting " ++ task ++ "."
    threadID <- forkIO $ logger maxI logVar
    return (threadID,logVar)

{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}
module Main where

import qualified Data.HashMap.Strict as M
import Data.Time.Clock (getCurrentTime,NominalDiffTime)
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
import Data.Text (Text)
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

dbthread :: Connection -> TBChan DBAction -> IO ()
dbthread conn chan = do
    action <- atomically $ readTBChan chan
    perform action
    where perform :: DBAction -> IO ()
          perform (DBWrite stmt sql) = executeMany stmt sql
          perform  Commit = do time <- doTimed_ $ commit conn
                               putStrLn $ "Commit took " ++ renderS time
          perform (Log event (name,fp)) = do
              logStatement <- prepare conn insertAction
              t <- getCurrentTime
              void $ execute logStatement [toSql event, toSql name, toSql fp, toSql t]

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

process :: PFEGConfig -> IO ()
process session = do
    tchan <- atomically $ newTBChan 10
    void $ forkIO . forever $ dbthread (database session) tchan
    case pfegMode session of
        Record{ corpora = cs } -> do
          s <- prepare (database session) insertSentence
          let it = sentenceIteratee (recordF tchan s)
          void $ workOnCorpora "record" tchan it session (0,[]) cs
        Match{ corpora = cs, resultLog = l } -> do
          chan <- newChan
          void $ forkIO (evalPFEG (forever $ matchLogger l chan) initialMatchScore session)
          let it = itemIteratee (getSentenceItems (`elem` targets session)) (matchF chan)
          void $ workOnCorpora "match" tchan it session () cs
        Predict { corpora = cs, resultLog = l } -> do
          chan <- newChan
          void $ forkIO (evalPFEG (forever $ matchLogger l chan) initialPredictScore session)
          let it = itemIteratee (getMaskedItems (`elem` targets session)) (matchF chan)
          void $ workOnCorpora "predict" tchan it session () cs
    putStrLn "Waiting for DB…" 
    atomically $ do
        r <- tryPeekTBChan tchan
        unless (isJust r) retry

matchF :: QueryChan -> ItemProcessor_
matchF log i = do
    session <- ask
    queries <- mapM (querify.Pat.makeQuery (snd i)) (matchPatterns session)
    liftIO $ do
        putStr "Querying…"
        (results,time) <- liftIO . doTimed $ runQueries (searchConf.pfegMode $ session) queries
        writeChan log $ QueryData i results time
        putStr "\r"

querify :: Text -> PFEG st Query
querify q = do index <- liftM sphinxIndex ask
               return Query { queryString = q, queryIndexes = index, queryComment = T.empty }

type QueryChan = Chan QueryData

data QueryData = QueryData
    { qItem    :: !(Item Text)
    , qResults :: !(Sphinx.Result [QueryResult])
    , qTime    :: !NominalDiffTime }
    deriving (Show)

initialPredictScore, initialMatchScore :: Score
initialPredictScore = PredictScore 0 0 0 0 0 0 0 0 0 0
initialMatchScore = MatchScore 0 0

tickScore :: (MonadState Score m) => m ()
tickScore = modify' $ \ s -> s { totalScored = totalScored s+1 }

whichCase :: Score -> Text -> Text -> Token Text -> Score
whichCase = undefined {-s@MatchScore { } majB p t
     | p == gold || p == majB = s { scoreCorrect = scoreCorrect s + 1 }
     | otherwise = s
whichCase s@PredictScore { } p gold orig alts
     | p == gold && p == orig = s { scoreAAA = scoreAAA s + 1, scoreCorrect = scoreCorrect s + 1 }
     | p == gold             = s { scoreAAB = scoreAAB s + 1, scoreCorrect = scoreCorrect s + 1 }
     | p == orig             = s { scoreABA = scoreABA s + 1 }
     | orig == gold          = s { scoreABB = scoreABB s + 1 }
     | p `elem` alts && p == orig = s { scoreAAAContained = scoreAAAContained s + 1
                                      , scoreCorrect      = scoreCorrect s      + 1 }
     | p `elem` alts              = s { scoreAABContained = scoreAABContained s + 1
                                      , scoreCorrect      = scoreCorrect s      + 1 }
     | orig `elem` alts           = s { scoreABBContained = scoreABBContained s + 1 }
     | otherwise = s { scoreABC = scoreABC s + 1 } -}

score :: Maybe Text -> Item Text -> PFEG Score Score
score p i = do
    mB <- liftM majorityBaseline ask
    let isCorrect = case p of
                         Nothing -> T.pack mB == surface (fst i)
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
         (Sphinx.Warning warn a) -> (a, Just $ T.concat ["WARNING: ", bs warn])
         (Sphinx.Ok a)           -> (a, Nothing)
         (Sphinx.Error code msg) -> ([], Just $ T.concat["ERROR (",T.pack . show $ code,"): ", bs msg])
         (Sphinx.Retry msg)      -> ([], Just $ T.concat["RETRY: ", bs msg])

type DocId = Int
type DocMap = M.HashMap DocId (Text,Text)

matchLogger :: Handle -> QueryChan -> PFEG Score ()
matchLogger l c = do
    session <- ask
    (item,time,sentences) <- liftIO $ retrieveSentences (database session) c
    let preds      = concatMap (getMatches (targets session) (matchPatterns session) $ snd item) sentences
        scoredData = scoreMatchData preds
        prediction = findBestPrediction (snd item) scoredData
    newScore <- score (fmap fst prediction) item
    put $! newScore
    liftIO $ do forM_ (map (showScoredData item (totalScored newScore)) scoredData) (\line -> hPutStrLn l line >> hFlush l)
                putStrLn $ "P: " ++ show (fmap (T.unpack . fst) prediction) ++ "\n\
                           \A: " ++ show (fst item) ++ "\n\
                           \T: " ++ renderS time ++ "\n\
                           \S: " ++ show newScore ++ "\n\
                           \X: " ++ show (fmap snd prediction)

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

retrieveSentences :: Connection -> QueryChan -> IO (Item Text, NominalDiffTime, [Sentence Text])
retrieveSentences conn c = do
    (QueryData item response time) <- readChan c
    let (results,msg) = getQueryResults response
        docids = map parseResult results
    when (isJust msg) $ putStrLn $ "WARNING: " ++ (T.unpack . fromJust $ msg)
    sentences <- queryDB conn . unique . concat $ docids
    return (item,time,sentences)

type ScoredMatchData = (Text,(Pat.MatchPattern,Double))

findBestPrediction :: Context (Token Text) -> [ScoredMatchData] -> Maybe ScoredMatchData
findBestPrediction _ =
    listToMaybe . sortBy (compare `on` (snd . snd)) . M.toList . M.fromListWith addWeights
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
getEx ec index docs qry = liftM getQueryResults $ buildExcerpts ec docs index qry

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
queryDB :: Connection -> [DocId] -> IO [Sentence Text]
queryDB conn ids = do
    let arg = (++")") . ('(':) . intercalate "," . map show $ ids
    sql <- liftIO $ quickQuery' conn ("SELECT surface,lemma,pos FROM records WHERE id in " ++ arg) []
    return $ map mkSentence sql
    where mkSentence :: [SqlValue] -> Sentence Text
          -- The reason we're using the explicit Word record syntax here is to never mix up surface, pos, and lemma
          -- as this can lead to embarrassing and difficult to detect bugs.
          mkSentence (s:l:p:[]) = zipWith3 (\s' l' p' -> Word { pos = p', lemma = l', surface = s' })
                                  (sqlw p) (sqlw l) (sqlw s)
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

workOnCorpora :: Text -> TBChan DBAction -> I.Iteratee (Sentence Text) (PFEG st) () -> PFEGConfig -> st -> [Corpus] -> IO [st]
workOnCorpora action tchan it session st = mapM $ \ c@(cName,cFile) -> do
    (threadID,logVar) <- evalPFEG (forkLogger c) () session
    let iteratee = I.run =<< enumFile (chunkSize session) cFile (I.sequence_
                 [ countChunksI logVar , I.joinI $ I.convStream corpusI it ])
    -- log to db that we're starting here
    atomically $ writeTBChan tchan (Log (T.unwords [action,"start"]) c)
    res <- execPFEG iteratee st session
    killThread threadID
    putStrLn $ "Finished " ++ cName
    -- log to db that we're finished here and commit
    atomically $ do writeTBChan tchan (Log (T.unwords [action,"end"]) c)
                    writeTBChan tchan Commit
    return res

recorder :: Statement -> MVar RecordData -> MVar () -> IO ()
recorder s mvar cmd = do
    takeMVar cmd
    vals <- takeMVar mvar
    void $ executeMany s vals
    putMVar cmd ()

type RecordData = [[SqlValue]]

recordF :: TBChan DBAction -> Statement -> SentenceProcessor (Int, RecordData)
recordF chan stmt s = do
    (i,vals) <- get
    let vals' = sentence2SQL s:vals
    if i == 10000
       then put (0,[]) >> liftIO (atomically (writeTBChan chan (DBWrite stmt vals')))
       else put (i+1,vals')

itemIteratee :: ItemGetter -> ItemProcessor st -> Iteratee (Sentence Text) (PFEG st) ()
itemIteratee gI proc = I.mapChunksM_ $ mapM proc . gI

sentenceIteratee :: SentenceProcessor st -> Iteratee (Sentence Text) (PFEG st) ()
sentenceIteratee = I.mapChunksM_

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

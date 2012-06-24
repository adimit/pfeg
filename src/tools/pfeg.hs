{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}
module Main where

import qualified Data.HashMap.Strict as M
import Data.Time.Clock (NominalDiffTime)
import qualified Text.Search.Sphinx.Types as Sphinx
import Text.Search.Sphinx.Types (QueryResult(..))
import Text.Search.Sphinx hiding (sortBy,mode)
import Text.Search.Sphinx.ExcerptConfiguration (ExcerptConfiguration)
import Data.Text.ICU hiding (compare,pattern)
import Data.Hashable (Hashable)

import Data.Maybe (mapMaybe)
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

import Text.Groom
import PFEG.Configuration
import qualified ReadArgs as RA
import qualified Data.HashSet as Set

import qualified PFEG.Pattern as Pat

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
process session =
    case pfegMode session of
        Record{ corpora = cs } -> do
          s <- prepare (database session) insertSentence
          mvar <- newEmptyMVar
          cmd <- newMVar ()
          void $ forkIO . forever $ recorder s mvar cmd
          let it = sentenceIteratee (recordF mvar)
          void $ workOnCorpora it session (0,[]) cs
          putStrLn "Waiting for DB…" >> takeMVar cmd
        Match{ corpora = cs, resultLog = l } -> do
          chan <- newChan
          void $ forkIO (evalPFEG (forever $ matchLogger l chan) initialMatchScore session)
          let it = itemIteratee (getSentenceItems (`elem` targets session)) (matchF chan)
          void $ workOnCorpora it session () cs
        Predict { corpora = cs, resultLog = l } -> do
          chan <- newChan
          void $ forkIO (evalPFEG (forever $ matchLogger l chan) initialPredictScore session)
          let it = itemIteratee (getMaskedItems (`elem` targets session)) (matchF chan)
          void $ workOnCorpora it session () cs

matchF :: QueryChan -> ItemProcessor_
matchF log i = do
    session <- ask
    queries <- mapM (querify.Pat.makeQuery (snd i)) patterns
    liftIO $ do
        putStr "Querying…"
        (results,time) <- liftIO . doTimed $ runQueries (searchConf.pfegMode $ session) queries
        writeChan log $ QueryData (map queryString queries) i results time
        putStr "\r"

querify :: Text -> PFEG st Query
querify q = do index <- liftM sphinxIndex ask
               return Query { queryString = q, queryIndexes = index, queryComment = T.empty }

type QueryChan = Chan QueryData

data QueryData = QueryData
    { qStrings :: ![Text]
    , qItem    :: !(Item Text)
    , qResults :: !(Sphinx.Result [QueryResult])
    , qTime    :: !NominalDiffTime }
    deriving (Show)

initialPredictScore, initialMatchScore :: Score
initialPredictScore = PredictScore 0 0 0 0 0 0 0 0 0 0
initialMatchScore = MatchScore 0 0

tickScore :: (MonadState Score m) => m ()
tickScore = modify' $ \ s -> s { totalScored = totalScored s+1 }

-- score :: Token Text -> [Prediction] -> PFEG Score (SphinxPattern,Text)
-- score t ps = do
--     tickScore
--     currentScore <- get
--     def <- liftM (T.pack . majorityBaseline) ask
--     let (bestPrediction,pattern) = findBestPrediction def ps
--     put $! case t of
--                 Masked { original = orig, surface = sfc, alternatives = alts } ->
--                                           whichCase currentScore bestPrediction sfc orig alts
--                 Word { surface = sfc } -> whichCase currentScore bestPrediction sfc T.empty []
--     return (pattern,bestPrediction)

whichCase :: Score -> Text -> Text -> Text -> [Text] -> Score
whichCase s@MatchScore { } p gold _orig alts
     | p == gold || p `elem` alts = s { scoreCorrect = scoreCorrect s + 1 }
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
     | otherwise = s { scoreABC = scoreABC s + 1 }

-- findBestPrediction :: Text -> [Prediction] -> (Text,SphinxPattern)
-- findBestPrediction majBase ps =
--     fromMaybe (majBase, MajorityBaseline) (listToMaybe . catMaybes $
--               zipWith f (map listToMaybe ps) patterns)
--     where f Nothing _ = Nothing
--           f (Just (x,_))  p = Just (x,p)

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
    (item,time,prediction) <- liftIO $ do
        (QueryData queries item response time) <- readChan c
        let (results, msg) = getQueryResults response
            docids = map parseResult results
        docMap <- queryDB (database session) . concat $ unique docids
        let docs = zipWith chooseSide queries (map (map $ lookupDoc docMap) docids)
        (excerpts,msgs) <- liftM unzip $
            zipWithM (getEx (exConf . pfegMode $ session) (sphinxIndex session)) docs queries
        mapM_ (maybe (return ()) (putStrLn . T.unpack)) (msg:msgs)
        let prediction = map (count . concatMap (getPrediction (mRegex . pfegMode $ session))) excerpts
        return (item,time,prediction)
    (winningPattern,bestPrediction) <- undefined -- score (fst item) prediction
    newScore <- get
    let ls = zipWith (logDataLine item (totalScored newScore) time) prediction patterns
    liftIO $ do forM_ ls (\line -> hPutStrLn l line >> hFlush l)
                putStrLn $ "P: " ++ T.unpack bestPrediction ++
                         "\nA: " ++ show (fst item) ++
                         "\nT: " ++ renderS time ++
                         "\nS: " ++ show newScore ++
                         "\nX: " ++ show (winningPattern :: Pat.MatchPattern)

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

-- | Get surface and lemma of documents from the DB in batch.
queryDB :: Connection -> [DocId] -> IO (M.HashMap DocId (Text,Text))
queryDB conn ids = do
    let arg = (++")") . ('(':) . intercalate "," . map show $ ids
    sql <- liftIO $ quickQuery' conn ("SELECT surface,lemma FROM records WHERE id in " ++ arg) []
    return . M.fromList $ zip ids (map fsql sql)
    where fsql (s:l:[]) = (fromSql s, fromSql l)
          fsql x        = error $ "SQL reply not in the right format:\n" ++ groom x

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

logDataLine :: Item Text -> Int -> NominalDiffTime -> Prediction -> Pat.MatchPattern -> String
logDataLine (w,Context { left = lc, right = rc }) x time ps p =
    intercalate "\t" $ [ show x                        -- item number
                       , T.unpack w        -- actually there
                       , show p ]                      -- pattern
                       ++ predictions ps               -- our predictions
                       ++ map untext [map surface lc,map surface rc] -- the item
                       ++ [ renderS time ]             -- the time the query took

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

-- TODO: add a Parser SphinxPattern so we can put patterns in the config

instance Show SphinxPattern where
    show MajorityBaseline = "majority baseline"
    show p = getLetter p : show (width p) ++ show (tolerance p)

-- testItem, testItem' :: Item String
-- testItem' = Item { itemSurface = Context ["ich","gehe"] ["die","schule", "in", "kazachstan"]
--                  , itemLemma   = Context ["ich","gehen"] ["d", "schule", "in", "kazachstan"]
--                  , target = Word "in" "in" "in" }
--
-- testItem  = Item { itemSurface = Context ["ich","gehe"] ["die","uni", "in", "kazachstan"]
--                  , itemLemma   = Context ["ich","gehen"] ["d", "uni", "in", "kazachstan"]
--                  , target = Word "auf" "auf" "auf" }

getLetter :: SphinxPattern -> Char
getLetter Surface {} = 's'
getLetter Lemma   {} = 'l'
getLetter MajorityBaseline = 'M'

wrap :: Char -> Text -> Text
wrap c = wrap2 c c

wrap2 :: Char -> Char -> Text -> Text
wrap2 a b t = T.cons a $ T.concat [t, T.singleton b]

patterns :: [Pat.MatchPattern]
patterns = undefined -- [ Surface x y | x <- [4,3,2,1], y <- [1..3] ] ++ [ Lemma x y | x <- [4,3,2,1] , y <- [1..3] ]

workOnCorpora :: I.Iteratee (Sentence Text) (PFEG st) () -> PFEGConfig -> st -> [Corpus] -> IO [st]
workOnCorpora it session st = mapM $ \ c@(cName,cFile) -> do
    (threadID,logVar) <- evalPFEG (forkLogger c) () session
    let iteratee = I.run =<< enumFile (chunkSize session) cFile (I.sequence_
                 [ countChunksI logVar , I.joinI $ I.convStream corpusI it ])
    res <- execPFEG iteratee st session
    killThread threadID
    putStrLn $ "Finished " ++ cName
    return res

recorder :: Statement -> MVar RecordData -> MVar () -> IO ()
recorder s mvar cmd = do
    takeMVar cmd
    vals <- takeMVar mvar
    void $ executeMany s vals
    putMVar cmd ()

type RecordData = [[SqlValue]]

recordF :: MVar RecordData -> SentenceProcessor (Int, RecordData)
recordF mvar s = do
    (i,vals) <- get
    let vals' = sentence2SQL s:vals
    if i == 10000
       then put (0,[]) >> liftIO (putMVar mvar vals')
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

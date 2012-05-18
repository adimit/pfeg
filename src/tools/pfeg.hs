{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}
module Main where

import qualified Data.HashMap.Strict as M
import Data.Time.Clock (NominalDiffTime)
import qualified Text.Search.Sphinx.Types as Sphinx
import Text.Search.Sphinx.Types (QueryResult(..))
import Text.Search.Sphinx hiding (sortBy,mode)
import Data.Text.ICU hiding (compare,pattern)

import Data.Maybe (mapMaybe,fromMaybe,listToMaybe,catMaybes)
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
          let it = standardIteratee (getItems $ targets session) (matchF chan)
          void $ workOnCorpora it session () cs
        Predict { corpora = cs, resultLog = l } -> do
          chan <- newChan
          void $ forkIO (evalPFEG (forever $ matchLogger l chan) initialPredictScore session)
          let it = standardIteratee (getMaskedItems' $ targets session) (matchF chan)
          void $ workOnCorpora it session () cs

matchF :: QueryChan -> ItemProcessor_
matchF log item = do
    session <- ask
    queries <- mapM (makeAQuery item) patterns
    liftIO $ do
        putStr "Querying…"
        (results,time) <- liftIO . doTimed $ runQueries (searchConf.pfegMode $ session) queries
        writeChan log $ QueryData (map queryString queries) item results time
        putStr "\r"

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

score :: Score -> Token Text -> [Prediction] -> PFEG a (Score,SphinxPattern,Text)
score s Masked { original = orig, surface = sfc, alternatives = alts } ps = do
    (bestPrediction,pattern) <- findBestPrediction ps
    return (whichCase s bestPrediction sfc orig alts,pattern,bestPrediction)
score s Word { surface = sfc } ps = do
    (bestPrediction,pattern) <- findBestPrediction ps
    return (whichCase s bestPrediction sfc T.empty [],pattern,bestPrediction)

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

findBestPrediction :: [Prediction] -> PFEG a (Text,SphinxPattern)
findBestPrediction ps = do
    def <- liftM (T.pack . majorityBaseline) ask
    return $ fromMaybe (def, MajorityBaseline) (listToMaybe . catMaybes $
             zipWith f (map listToMaybe ps) patterns)
    where f Nothing _ = Nothing
          f (Just (x,_))  p = Just (x,p)

bs :: LB.ByteString -> Text
bs = decodeUtf8 . SB.concat . LB.toChunks

getQueryResults :: Sphinx.Result [QueryResult] -> ([QueryResult],Maybe Text)
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
    tickScore
    (QueryData queries item result time) <- liftIO $ readChan c
    let (results, msg) = getQueryResults result
        docids = map parseResult results
    liftIO $ maybe (return ()) (putStrLn . T.unpack) msg
    docMap <- queryDB $ concat docids
    let doqData = zip queries (map (map $ lookupDoc docMap) docids)
    targetPredictions <- mapM matchTargets doqData
    currentScore <- get
    (newScore,winningPattern,bestPrediction) <- score currentScore (target item) targetPredictions
    put $! newScore
    ls <- mapM (uncurry $ logDataLine item time) (zip targetPredictions patterns)
    liftIO $ do forM_ ls (\line -> hPutStrLn l line >> hFlush l)
                putStrLn $ "P: " ++ T.unpack bestPrediction ++
                         "\nA: " ++ show (target item) ++
                         "\nT: " ++ renderS time ++
                         "\nS: " ++ show newScore ++
                         "\nX: " ++ show winningPattern

lookupDoc :: DocMap -> DocId -> (Text,Text)
lookupDoc = flip (M.lookupDefault (T.empty,T.empty))

matchTargets :: (Text,[(Text,Text)]) -> PFEG a Prediction
matchTargets (q,res) = do
    session <-  ask
    let res' = chooseSide q res
    reply <- liftIO $ buildExcerpts (exConf . pfegMode $ session) res' (sphinxIndex session) q
    let excerpts = case reply of
                        Sphinx.Ok a -> a
                        _           -> []
    p <- mapM getPrediction excerpts
    return $ sortBy (compare `on` snd) . M.toList . M.fromListWith (+) $ zip (concat p) (repeat 1)


-- TODO: generalize to not only just find the first match in a sentence
-- It IS unlikely that a given query will match twice and with different
-- results, but not impossible.
getPrediction :: Text -> PFEG a [Text]
getPrediction exc = do
    rex <- liftM (mRegex . pfegMode) ask
    return . mapMaybe (group 1) . findAll rex $ exc

chooseSide :: Text          -- ^ The query string
             -> [(Text,Text)] -- ^ Documents with surface and lemma
             -> [Text]        -- ^ Either surface or lemma, depending on the query
chooseSide q d | "@lemma" `T.isPrefixOf` q = map snd d
               | otherwise                 = map fst d

queryDB :: [DocId] -> PFEG a (M.HashMap DocId (Text,Text))
queryDB ids = do
    let uniqueIds = Set.toList . Set.fromList $ ids
        arg = (++")") . ('(':) . intercalate "," . map show $ uniqueIds
    db <- liftM database ask
    sql <- liftIO $ quickQuery' db ("SELECT surface,lemma FROM records WHERE id in " ++ arg) []
    return . M.fromList $ zip uniqueIds (map fsql sql)
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

logDataLine :: Item Text -> NominalDiffTime -> Prediction -> SphinxPattern -> PFEG Score String
logDataLine Item { itemLemma = Context lcl rcl
                 , itemSurface = Context lcs rcs
                 , target = w } time ps p = do
    x <- liftM totalScored get
    return $ intercalate "\t" $
        [ show x                        -- item number
        , T.unpack . surface $ w        -- actually there
        , show p ]                      -- pattern
        ++ predictions ps               -- our predictions
        ++ map untext [lcl,rcl,lcs,rcs] -- the item
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

testItem, testItem' :: Item String
testItem' = Item { itemSurface = Context ["ich","gehe"] ["die","schule", "in", "kazachstan"]
                 , itemLemma   = Context ["ich","gehen"] ["d", "schule", "in", "kazachstan"]
                 , target = Word "in" "in" "in" }

testItem  = Item { itemSurface = Context ["ich","gehe"] ["die","uni", "in", "kazachstan"]
                 , itemLemma   = Context ["ich","gehen"] ["d", "uni", "in", "kazachstan"]
                 , target = Word "auf" "auf" "auf" }

getLetter :: SphinxPattern -> Char
getLetter Surface {} = 's'
getLetter Lemma   {} = 'l'
getLetter MajorityBaseline = 'M'

getContext :: Item a -> SphinxPattern -> ([a],[a])
getContext Item { itemSurface = (Context ls rs) } (Surface {width = w}) = makeContext ls rs w
getContext Item { itemLemma   = (Context ls rs) } (Lemma {width = w})   = makeContext ls rs w
getContext _ MajorityBaseline = ([],[])

makeContext :: [a] -> [a] -> Int -> ([a],[a])
makeContext ls rs i = (reverse $ take i (reverse ls),take i rs)

makeAQuery :: Item Text -> SphinxPattern -> PFEG a Query
makeAQuery item pattern = do
    let mkQ :: Item Text -> SphinxPattern -> PFEG a Text
        mkQ Item { itemSurface = c } p@Surface { } = makeQuery' c "@surface" p
        mkQ Item { itemLemma   = c } p@Lemma   { } = makeQuery' c "@lemma"   p
        mkQ _ x = error $ "Pattern type "++ show x ++" not suited for query construction."
    q <- mkQ item pattern
    index <- liftM sphinxIndex ask
    return Query { queryString = q, queryIndexes = index, queryComment = T.empty }

makeQuery' :: Context Text -> Text -> SphinxPattern -> PFEG a Text
makeQuery' c level p = do
    ts <- liftM targets ask
    let w = width p
        leftContext  = reverse . take w . reverse . left $ c
        rightContext = take w . right $ c
        prox x       = tolerance p + 2 * length (filter (`elem` ts) x)
    -- TODO: Replace << with NEAR, though watch out for the silly tag-counts-as-position bug.
    return $ T.intercalate " " [level, mkC prox leftContext, "<<", mkC prox rightContext]
    where mkC prox x = T.concat 
              [ wrap '"' . T.unwords $ x
              , let p' = prox x in if p' <= 1 then T.empty else T.pack $ '~':show p' ]

wrap :: Char -> Text -> Text
wrap c x = T.cons c $ T.concat [x, T.singleton c]

patterns :: [SphinxPattern]
patterns = [ Surface x y | x <- [4,3,2,1], y <- [1..3] ] ++ [ Lemma x y | x <- [4,3,2,1] , y <- [1..3] ]

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

standardIteratee :: ItemGetter -> ItemProcessor st -> Iteratee (Sentence Text) (PFEG st) ()
standardIteratee gI proc = I.mapChunksM_ $ mapM proc . gI

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

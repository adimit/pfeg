{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Main where

import qualified Text.Search.Sphinx.Types as Sphinx
import Text.Search.Sphinx.Types (QueryResult(..))
import Text.Search.Sphinx hiding (sortBy,mode)
import Data.Binary.Put (Put)

import qualified Data.ByteString.Lazy.Char8 as B
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

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent (ThreadId,killThread,forkIO)
import Control.Exception (bracket)

import Control.Monad.State.Strict
import Control.Monad.Reader

import Database.HDBC
import Database.HDBC.MySQL

import Graphics.Vty.Terminal

import PFEG.Configuration
import qualified ReadArgs as RA

main :: IO ()
main = do
    (mode :: String, configFile :: FilePath) <- RA.readArgs
    withRTSSignalsBlocked $
      bracket (do putStrLn "Initializing…"
                  terminal_handle >>= hide_cursor
                  configAttempt <- configurePFEG mode configFile
                  case configAttempt of
                       (Left err) -> error $ "Initialization failed: "++ show err
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
          s <- prepare (database session) upsertRecord
          mvar <- newEmptyMVar
          cmd <- newMVar ()
          void $ forkIO . forever $ recorder s mvar cmd
          let it = standardIteratee (recordF mvar) session
          void $ workOnCorpora it session (0,[]) cs
          putStrLn "Waiting for DB…" >> takeMVar cmd
        Match{ corpora = cs, resultLog = l } -> do
          chan <- newChan
          void $ forkIO (evalStateT (matchLogger l chan) initialMatcherState)
          let it = standardIteratee (matchF chan) session
          void $ workOnCorpora it session () cs
        Predict { } -> undefined

matchLogger :: Handle -> Chan (Item Text, Sphinx.Result [QueryResult]) -> StateT MatcherState IO ()
matchLogger l c = do
    next
    (item,result) <- liftIO $ readChan c
    results <- case result of
         (Sphinx.Warning warn a) -> liftIO (putStrLn $ "WARNING: " ++ B.unpack warn) >> return a
         (Sphinx.Ok a)           -> return a
         (Sphinx.Error code msg) -> liftIO (putStrLn $ "ERROR ("++ show code++"): " ++ B.unpack msg) >> return []
         (Sphinx.Retry msg)      -> liftIO (putStrLn $ "RETRY: " ++ B.unpack msg) >> return []
    let preds = zip (map parseResult results) patterns
        log' = uncurry $ logDataLine item
    ls <- mapM log' preds
    liftIO $ forM_ ls (\line -> hPutStr l line >> hFlush l)

logDataLine :: Item Text -> Prediction -> SphinxPattern -> StateT MatcherState IO String
logDataLine Item { itemLemma = Context lcl rcl
                 , itemSurface = Context lcs rcs
                 , target = w } ps p = do
    (MatcherState x _) <- get
    return $ intercalate "\t" $
        [ show x                        -- item number
        , T.unpack . surface $ w        -- actually there
        , show p ]                      -- pattern
        ++ predictions ps               -- our predictions
        ++ map untext [lcl,rcl,lcs,rcs] -- the item

-- first three predictions
predictions :: Prediction -> [String]
predictions ps = take 3 $ concatMap (\ (a,b) -> [T.unpack a,show b] ) ps ++ repeat ""

untext :: [Text] -> String
untext = T.unpack . T.unwords

next, correct :: MonadState MatcherState m => m ()
next    = modify' $ \ (MatcherState t c) -> MatcherState (t+1) c
correct = modify' $ \ (MatcherState t c) -> MatcherState t (c+1)

data Score = Score
    { totalScored       :: !Int
    , scoreAAA          :: !Int
    , scoreAAB          :: !Int
    , scoreABA          :: !Int
    , scoreABB          :: !Int
    , scoreABC          :: !Int
    , scoreAAAContained :: !Int
    , scoreAABContained :: !Int
    } deriving Show

{-score :: Score -> Token Text -> Prediction -> Score
score s Masked { original = orig, surface = sfc, alternatives = alts } ps = undefined

nullScore :: Score
nullScore = undefined -}

type Prediction = [(Text,Int)]
data MatcherState = MatcherState { totalMatches :: !Int, correctMatches :: !Int }

parseResult :: QueryResult -> Prediction
parseResult (QueryResult { matches = ms }) = sortBy (flip compare `on` snd) $ map getMatch ms
    where getMatch :: Sphinx.Match -> (Text,Int)
          getMatch Sphinx.Match { Sphinx.attributeValues = attrs } =
             case attrs of
                  (Sphinx.AttrString t:_:Sphinx.AttrUInt c:[]) -> (T.pack . B.unpack $ t, c)
                  _ -> error $ "Malformed search result: " ++ show attrs

initialMatcherState :: MatcherState
initialMatcherState = MatcherState 0 0

correctPrediction :: (MonadState MatcherState m) => m ()
correctPrediction = modify' (\MatcherState { totalMatches = t, correctMatches = c } ->
                              MatcherState (t+1) (c+1))

matchF :: Chan (Item Text, Sphinx.Result [QueryResult]) -> ItemProcessor_
matchF log item = do
    session <- ask
    queries <- mapM (makeAQuery item) patterns
    results <- liftIO $ runQueries (searchConf.pfegMode $ session) queries
    liftIO $ writeChan log (item,results)

data SphinxPattern = Surface { width :: !Int, tolerance :: !Int }
                   | Lemma   { width :: !Int, tolerance :: !Int }

-- TODO: add a Parser SphinxPattern so we can put patterns in the config

instance Show SphinxPattern where
    show p = getLetter p : show (width p) ++ show (tolerance p)

testItem, testItem' :: Item String
testItem' = Item { itemSurface = Context ["ich","gehe"] ["die","schule"]
                 , itemLemma   = Context ["ich","gehen"] ["d", "schule"]
                 , target = Word "in" "in" "in" }

testItem  = Item { itemSurface = Context ["ich","gehe"] ["die","uni"]
                 , itemLemma   = Context ["ich","gehen"] ["d", "uni"]
                 , target = Word "auf" "auf" "auf" }

getLetter :: SphinxPattern -> Char
getLetter Surface {} = 's'
getLetter Lemma   {} = 'l'

makeQuery :: Item Text -> SphinxPattern -> String
makeQuery i p =
    mkC 'l' (unText lc) ++ " " ++ mkC 'r' (unText rc)
    where (lc,rc) = getContext i p
          mkC side c = '@':side:'c':getLetter p:" \"" ++ c ++ "\"" ++ tol
          unText = T.unpack . T.unwords
          tol = case tolerance p of
                0 -> ""
                x -> '~':show x

getContext :: Item a -> SphinxPattern -> ([a],[a])
getContext Item { itemSurface = (Context ls rs) } (Surface {width = w}) = makeContext ls rs w
getContext Item { itemLemma   = (Context ls rs) } (Lemma {width = w})   = makeContext ls rs w
makeContext :: [a] -> [a] -> Int -> ([a],[a])
makeContext ls rs i = (reverse $ take i (reverse ls),take i rs)

makeAQuery :: Item Text -> SphinxPattern -> PFEG a Put
makeAQuery item pattern = do
    let q = makeQuery item pattern
    conf <- liftM (searchConf.pfegMode) ask
    return $ addQuery conf q "*" (show pattern)

patterns :: [SphinxPattern]
patterns = [ Surface x y | x <- [3,2,1], y <- [0..2] ] ++ [ Lemma x y | x <- [3,2] , y <-[0..1] ]

workOnCorpora :: I.Iteratee (Sentence Text) (PFEG st) () -> PFEGConfig -> st -> [Corpus] -> IO [st]
workOnCorpora it session st = mapM $ \ c@(_cName,cFile) -> do
    (threadID,logVar) <- evalPFEG (forkLogger c) () session
    let iteratee = I.run =<< enumFile (chunkSize session) cFile (I.sequence_
                 [ countChunksI logVar , I.joinI $ I.convStream corpusI it ])
    res <- execPFEG iteratee st session
    killThread threadID
    return res

recorder :: Statement -> MVar RecordData -> MVar () -> IO ()
recorder s mvar cmd = do
    takeMVar cmd
    vals <- takeMVar mvar
    void $ executeMany s vals
    putMVar cmd ()

type RecordData = [[SqlValue]]

recordF :: MVar RecordData -> ItemProcessor (Int, RecordData)
recordF mvar item = do
    (i,vals) <- get
    let vals' = item2SQL item:vals
    if i == 10000
       then put (0,[]) >> liftIO (putMVar mvar vals')
       else put (i+1,vals')

standardIteratee :: ItemProcessor st -> PFEGConfig -> Iteratee (Sentence Text) (PFEG st) ()
standardIteratee proc session = I.mapChunksM_ $ mapM proc . getItems (targets session)

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

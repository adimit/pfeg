{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Main where

import PFEG.Types
import PFEG.SQL
import PFEG.Common
import PFEG.Context

import Prelude hiding (log)

import System.Time.Utils (renderSecs)
import Data.List (elemIndex,foldl',intercalate)

import Data.Iteratee.Base
import qualified Data.Iteratee as I
import Data.Iteratee.IO
import Data.Maybe (fromMaybe)

import System.IO

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent (ThreadId,killThread,forkIO)
import Control.Exception (bracket)

import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.State.Strict
import Control.Monad.Reader

import Database.HDBC
import Database.HDBC.PostgreSQL

import Graphics.Vty.Terminal

import PFEG.Configuration
import ReadArgs

import Codec.Digest.SHA

import Data.IORef

newtype PFEG a = PFEG { runP :: ReaderT PFEGConfig IO a }
        deriving (Functor, Monad, MonadIO, MonadReader PFEGConfig, MonadCatchIO)

data LogData = LogData
    { logItem    :: !(Item Text)
    , logResults :: [(MatchPattern,Result)] }

main :: IO ()
main = do
    (mode :: String, configFile :: FilePath) <- readArgs
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

data LogState = LogState { currentItem :: !Int }

logResult :: String -> Handle -> MVar LogData -> StateT LogState IO ()
logResult majB h resV = forever log -- who wants to be forever log
    where log = do (LogState n) <- get
                   (LogData item results) <- liftIO $ takeMVar resV
                   liftIO $ mapM_ (logDataLine majB h n item) results
                   put (LogState $ n+1)

logDataLine :: String -> Handle -> Int -> Item Text -> (MatchPattern, Result) -> IO ()
logDataLine majB h i (Item (Context pI) (Context lI) (Context sI) t) (pattern,result) =
    hPutStrLn h line >> hFlush h
    where line = intercalate "\t" $ [show i, unwrap sI, unwrap lI, unwrap pI, T.unpack t, show pattern] ++ res
          unwrap = unwords.map T.unpack
          showResult (prediction,count,ctxts) = [T.unpack prediction,show count, show ctxts]
          res = case result of
                     [] -> ["Baseline",majB] -- empty result, predict majority baseline
                     xs -> "Prediction":concatMap showResult xs

type ItemProcessor = Item Text -> PFEG ()

workOnCorpora :: ItemProcessor -> PFEGConfig -> [Corpus] -> IO ()
workOnCorpora processor session = mapM_ (handleCorpus processor session)

runPFEG :: PFEG a -> PFEGConfig -> IO a
runPFEG k = runReaderT (runP k)

handleCorpus :: ItemProcessor -> PFEGConfig -> Corpus -> IO ()
handleCorpus proc session c@(_cName,cFile) = do
     (threadID,logVar) <- runPFEG (forkLogger c) session
     let iteratee = I.run =<< enumFile (chunkSize session) cFile (I.sequence_
                        [ countChunksI logVar
                        , I.joinI $ I.convStream corpusI (I.mapChunksM_ $ mapM proc.getItems (targets session))])
     runPFEG iteratee session
     killThread threadID
     case pfegMode session of
          Record{} -> commitTo $ database session
          _        -> return ()

commitTo :: Connection -> IO ()
commitTo conn = do
     putStr "\nCommitting…" >> hFlush stdout
     time <- doTimed_ $ commit conn
     putStrLn $ "\rCommitted in "++ (renderSecs.round $ time)

process :: PFEGConfig -> IO ()
process session =
    case pfegMode session of
        m@Record{} -> do
            statement <- prepare (database session) upsertRecord
            mvar <- newEmptyMVar
            cmd <- newMVar ()
            void $ forkIO . forever $ recorder mvar cmd statement
            ioref <- newIORef (0,[])
            workOnCorpora (recordF ioref mvar) session (corpora m)
            putStrLn "Waiting for DB…" >> takeMVar cmd
        m@Match{} -> do
            sql <- undefined -- precompileSQL mkMatchSQL (database session) matchmodes
            logVar <- newEmptyMVar
            threadID <- forkIO . void $
                runStateT (logResult (majorityBaseline m) (resultLog m) logVar) (LogState 1)
            workOnCorpora (matchF logVar sql) session (corpora m)
            killThread threadID
        Unigrams{} -> do
            statement <- prepare (database session) upsertUnigram
            runPFEG (runUnigram statement) session

runUnigram :: Statement -> PFEG ()
runUnigram upsert = do
    session <- ask
    void . liftIO $ execute upsert [toSql nullToken, toSql (1::Int)]
    hist <- foldM acquireHistogram M.empty (corpora.pfegMode $ session)
    liftIO $ do putStr "Waiting for DB…" >> hFlush stdout
                t <- doTimed_ $ executeMany upsert (map (\ (k,v) -> [toSql k, toSql v]) (M.toList hist))
                putStrLn $ "\rDB took " ++ renderS t ++ "         "
                commitTo $ database session

histogramCommitter :: Statement -> MVar (Maybe Histogram) -> IO ()
histogramCommitter upsert histVar = loop
    where loop = do
          chanData <- takeMVar histVar
          case chanData of
               Nothing -> return ()
               (Just hist) -> executeMany upsert
                    (map (\ (k,v) -> [toSql k, toSql v]) (M.toList hist)) >> loop

{- TODO: this has to be merged somehow with handleCorpus, which does something pretty similar. -}
acquireHistogram :: Histogram -> Corpus -> PFEG Histogram
acquireHistogram hist c@(cName,cFile) = do
    session <- ask
    (threadID,logVar) <- forkLogger c
    liftIO $ do
        let iteratee = I.run =<< enumFile (chunkSize session) cFile (I.sequence_
                       [ countChunksI logVar, I.joinI $ I.convStream corpusI uniI ])

        histogram <- execStateT iteratee hist
        killThread threadID
        putStrLn $ "\nDone processing " ++ cName
        return histogram

forkLogger :: Corpus -> PFEG (ThreadId,Chan Int)
forkLogger (cName,cFile) = do
    session <- ask
    liftIO $ do
        logVar <- newChan
        csize  <- withFile cFile ReadMode hFileSize
        putStrLn $ "Processing '" ++ cName ++ "' at '" ++ cFile ++ ".'"
        threadID <- forkIO $ logger (fromIntegral csize `div` chunkSize session) logVar
        return (threadID,logVar)

-- | A strict update to monad state via @f@.
modify' :: (Monad m) => (s -> s) -> StateT s m ()
modify' f = do
    s <- get
    put $! f s

type Histogram = M.HashMap Text Int
uniI :: I.Iteratee (Sentence Text) (StateT Histogram IO) ()
uniI = I.liftI step
    where step (Chunk sent) = do lift $ modify' (treatSentence sent)
                                 I.liftI step
          step stream       = I.idone () stream
          treatSentence :: Sentence Text -> Histogram -> Histogram
          treatSentence sent hist = foldl' (flip f) hist (concatMap toList sent)
          f t = M.insertWith (+) t 1

toList :: (a,a,a) -> [a]
toList (a,b,c) = [a,b,c]

targetNo :: Text -> PFEG Int
targetNo t = do
    session <- ask
    return $ 1+fromMaybe (error $ "Unknown target '" ++ T.unpack t ++ "' in " ++ show (targets session))
                         (t `elemIndex` targets session)

recorder :: MVar [[SqlValue]] -> MVar () -> Statement -> IO ()
recorder mvar cmd statement = takeMVar cmd >> takeMVar mvar >>= executeMany statement >> putMVar cmd ()

{- FIXME: this is a dirty IORef hack and should probably be put into StateT instead, but I really
 - don't have time and initiative to do so now. -}
recordF :: IORef (Int,[[SqlValue]]) -> MVar [[SqlValue]] -> ItemProcessor
recordF ioref mvar item@Item{target = t} = do
    tn <- targetNo t
    (i,vals) <- liftIO $ readIORef ioref
    let vals' = [ toSql . showBSasHex . hash SHA256 $ item
                , toSql tn , item2SQL item]:vals
    if i == 1000
       then liftIO $ do putMVar mvar vals'
                        writeIORef ioref (0,[])
       else liftIO $ writeIORef ioref (i+1,vals')

matchF :: MVar LogData -> MatcherInit -> ItemProcessor
matchF logVar sql i = do
    results <- mapM (matchAPattern sql i) matchmodes
    liftIO $ putMVar logVar (LogData i (zip matchmodes results))

-- given a pattern, matcherInit and an Item, give a result from the database
-- helper function for @matchF@.
matchAPattern :: MatcherInit -> Item Text -> MatchPattern -> PFEG Result
matchAPattern sql i mm = do
    let pattern = undefined -- item2SQLp mm (indexItem uids i)
    case mm `M.lookup` sql of
         Nothing -> error "IMPOSSIBRU!"
         (Just s) -> do
             void $ liftIO $ execute s pattern
             rows <- liftIO $ fetchAllRows' s
             return $ foldl f [] rows
             where f r (t:c:idc:[]) = undefined -- (tids IM.! fromSql t,fromSql c,fromSql idc):r
                   f _ xs           = error $ "Unexpected data format." ++ show xs

-- FIXME: this needs a pretty printer
instance Show LogData where
    show (LogData (Item _ _ (Context s) t) lr) = intercalate " | " (map T.unpack s) ++ "\nActual: " ++ T.unpack t
        ++ "\n" ++ concatMap (\(mm,r) -> show mm ++ ": " ++ show r ++ "\n") lr

-- | list of possible predictions with associated counts and matches.
-- (target,count,amount_of_matches)
type Result = [(Text,Int,Int)]

-- | Map from @MatchPattern@s to SQL @Statement@s for the matcher.
type MatcherInit = HashMap MatchPattern Statement

precompileSQL :: (MatchPattern -> String) -> Connection -> [MatchPattern] -> IO MatcherInit
precompileSQL sqlMM conn = foldM f M.empty
    where f m mm = liftM (\s -> M.insert mm s m) (prepare conn (sqlMM mm))

-- preliminary list of matchmodes
matchmodes :: [MatchPattern]
matchmodes = map MatchPattern
             [ map Just [S,S,S,S,S,S]
             , map Just [L,L,L,L,L,L]
             , map Just [P,P,P,P,P,P]
             , Nothing : map Just [S,S,S,S]
             , Nothing : map Just [L,L,L,L]
             , Nothing : map Just [P,P,P,P]
             , Nothing : Nothing : map Just [S,S]
             , Nothing : Nothing : map Just [L,L]
             , Nothing : Nothing : map Just [P,P] ]

{-# LANGUAGE ScopedTypeVariables,BangPatterns #-}
module Main where

import PFEG.SQL
import PFEG.Common
import PFEG.Context

import Prelude hiding (log)

import System.Time.Utils (renderSecs)
import Data.List (intercalate)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import qualified Data.Iteratee as I
import Data.Iteratee.IO
import Data.Iteratee.Base
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)

import System.IO

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent (killThread,forkIO)
import Control.Exception (bracket)

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when,forever,liftM,foldM,when,void)

import Database.HDBC
import Database.HDBC.PostgreSQL

import Graphics.Vty.Terminal

import PFEG.Configuration
import ReadArgs

type PFEG a = ReaderT PFEGConfig IO a

data LogData = LogData
    { logItem    :: Item Text
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

data SQL = RecordSQL { updateTarget  :: Statement
                     , insertContext :: Statement
                     , insertTarget  :: Statement }

indexItem :: UnigramIDs -> Item Text -> Item Int
indexItem udb i = (fromMaybe 1 . (`M.lookup` udb)) `fmap` i

countChunksI' :: Chan Int -> Iteratee ByteString (ReaderT PFEGConfig IO) ()
countChunksI' log = I.liftI (step 0)
    where step (!i) (Chunk _) = let i' = i+1
                                in liftIO (writeChan log i') >> I.liftI (step i')
          step _    stream    = I.idone () stream

data LogState = LogState { currentItem :: Int }

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

handleCorpus :: ItemProcessor -> PFEGConfig -> Corpus -> IO ()
handleCorpus proc session (cName,cFile) = do
     logVar <- newChan
     csize  <- withFile cFile ReadMode hFileSize
     putStrLn $ "Processing '" ++ cName ++ "' at '" ++ cFile ++ ".'"
     threadID <- forkIO $ logger (fromIntegral csize `div` chunkSize session) logVar
     let iteratee = I.run =<< enumFile (chunkSize session) cFile (I.sequence_
                        [ countChunksI' logVar
                        , I.joinI $ I.convStream corpusI (I.mapChunksM_ $ mapM proc.getItems (targets session))])
     runReaderT iteratee session
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
            insertCtxtS <- prepare (database session) insertCtxtSQL
            insertTrgtS <- prepare (database session) insertTargetSQL
            updateS     <- prepare (database session) updateSQL
            let sql = RecordSQL { updateTarget  = updateS
                                , insertContext = insertCtxtS
                                , insertTarget  = insertTrgtS }
            workOnCorpora (recordF (unigramIDs m) sql) session (corpora m)
        m@Match{} -> do
            sql <- precompileSQL mkMatchSQL (database session) matchmodes
            logVar <- newEmptyMVar
            threadID <- forkIO . void $
                runStateT (logResult (majorityBaseline m) (resultLog m) logVar) (LogState 1)
            workOnCorpora (matchF (unigramIDs m) logVar (targetIDs m) sql) session (corpora m)
            killThread threadID
        Unigrams{} -> do
            updateUnigramS <- prepare (database session) updateUnigram
            insertUnigramS <- prepare (database session) updateUnigram
            runReaderT (runUnigram insertUnigramS updateUnigramS) session

runUnigram :: Statement -> Statement -> PFEG ()
runUnigram = undefined

recordF :: UnigramIDs -> SQL -> Item Text -> PFEG ()
recordF uids sql i = do
    let item'    = indexItem uids i
        pattern  = item2SQL item' -- just the slp forms
        pattern' = toSql (target item'):pattern -- the slp forms with target prepended
    numRows <- liftIO $ execute (updateTarget sql) pattern'
    when (numRows == 0) (do
         void.liftIO $ execute (insertContext sql) pattern
         void.liftIO $ execute (insertTarget  sql) pattern')

matchF :: UnigramIDs -> MVar LogData -> IntMap Text -> MatcherInit -> Item Text -> PFEG ()
matchF uids logVar tids sql i = do
    results <- mapM (matchAPattern uids tids sql i) matchmodes
    liftIO $ putMVar logVar (LogData i (zip matchmodes results))

-- given a pattern, matcherInit and an Item, give a result from the database
-- helper function for @matchF@.
matchAPattern :: UnigramIDs -> IntMap Text -> MatcherInit -> Item Text -> MatchPattern -> PFEG Result
matchAPattern uids tids sql i mm = do
    let pattern = item2SQLp mm (indexItem uids i)
    case mm `M.lookup` sql of
         Nothing -> error "IMPOSSIBRU!"
         (Just s) -> do
             void $ liftIO $ execute s pattern
             rows <- liftIO $ fetchAllRows' s
             return $ foldl f [] rows
             where f r (t:c:idc:[]) = (tids IM.! fromSql t,fromSql c,fromSql idc):r
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

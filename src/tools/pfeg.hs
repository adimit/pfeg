{-# LANGUAGE BangPatterns,DeriveDataTypeable #-}
module Main where

import PFEG.SQL
import PFEG.Types
import PFEG.Common
import PFEG.Context

import System.Time.Utils (renderSecs)
import Data.List.Split (splitOn)

import qualified Data.Iteratee as I
import Data.Iteratee (Iteratee)
import Data.Iteratee.IO
import Data.Iteratee.Base
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)

import System.IO (hFileSize,withFile,IOMode(ReadMode))

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)

import Data.Time.Clock

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent (forkIO,threadDelay)
import Control.Exception (bracket)

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when,void,forever)

import Database.HDBC
import Database.HDBC.Sqlite3

import System.Console.CmdArgs

import Graphics.Vty.Terminal

{- START CMDARGS GIBBERISH -}

data PFEGMain = Record { corpus    :: FilePath
                       , unigrams  :: FilePath
                       , database  :: FilePath
                       , sqlLog    :: Maybe FilePath
                       , targets   :: String
                       , shard     :: Int }
              | Match  { corpus    :: FilePath
                       , unigrams  :: FilePath
                       , database  :: FilePath
                       , sqlLog    :: Maybe FilePath
                       , targets   :: String
                       , shard     :: Int
                       , resultLog :: FilePath }
              deriving (Data, Typeable)

instance Show PFEGMain where
    show (Record c u db sql ts i)   = standardMessage "Recording to" c u db sql ts i ""
    show (Match  c u db sql ts i r) = standardMessage "Matching aigainst" c u db sql ts i ("Logging result to '" ++ r ++ "'")

standardMessage :: String -> FilePath -> FilePath -> FilePath ->
                  Maybe FilePath -> String -> Int -> String -> String
standardMessage m c u db sql ts i r = m ++ " '" ++ db ++ "'\n" ++
                                      "from corpus '" ++ c ++ "', shard " ++ show i ++ ".\n" ++
                                      "unigrams are '" ++ u ++ "'\n" ++ r ++ "\n" ++
                                      "targets are '" ++ show ts ++ "'\n" ++
                                      case sql of
                                           Nothing  -> ""
                                           (Just x) -> "Logging SQL to '" ++ x ++ "'.\n"

recordCmd, matchCmd :: PFEGMain
recordCmd = Record { corpus    = commonCorpus def
                   , unigrams  = commonUnigrams "../db/de/uni.db"
                   , database  = commonDatabase "../db/de/ctx.db"
                   , sqlLog    = commonSqlLog def
                   , targets   = commonTargets
                   , shard     = commonShard 1 }
                              &= help "Learn contexts from corpus and store in DB."

matchCmd  = Match  { corpus    = commonCorpus def
                   , unigrams  = commonUnigrams "../db/de/uni.db"
                   , database  = commonDatabase "../db/de/ctx.db"
                   , sqlLog    = commonSqlLog def
                   , targets   = commonTargets
                   , shard     = commonShard def
                   , resultLog = "result.log" &= typ "FILE" &= help "Result log file location."}
                              &= help "Match context from corpus against DB."

commonTargets = "in,von,mit,für,im,auf,nach,an,aus,am"
    &= typ "TARGETLIST" &= help "Comma-separated list of targets, e.g 'in,von,mit…'"

commonShard, commonCorpus, commonUnigrams, commonDatabase, commonSqlLog :: Data v => v -> v
commonCorpus   x = x &= typ "FILE" &= help "Input corpus in TT format."
commonUnigrams x = x &= typ "FILE" &= help "Location of the unigram index database."
commonDatabase x = x &= typ "FILE" &= help "Location of the Context database."
commonShard    x = x &= typ "INT"  &= help "Index of the current shard."
commonSqlLog   x = x &= typ "FILE" &= help "Output of SQL log, if desired."

mode :: Mode (CmdArgs PFEGMain)
mode = cmdArgsMode $ modes [matchCmd,recordCmd]
    &= help "Predicting Functional Elements in German (and others)"
    &= program "pfeg"
    &= summary "PFEG 0.1, Aleksandar Dimitrov 2012"

main :: IO ()
main = (\m -> print m >> handle m) =<< cmdArgsRun mode

{- END CMDARGS GIBBERISH -}

cacheHash :: Statement -> StateT UnigramIDs IO ()
cacheHash s = liftIO (void $ execute s []) >> fetchAll
    where fetchAll = do
          row <- liftIO $ fetchRow s
          case row of
               Just (f:i:[]) -> do m <- get
                                   put $! M.insert (fromSql f) (fromSql i) m
                                   fetchAll
               Nothing       -> return ()
               _             -> fail "Malformed result in unigrams."

data LogState = LogState { lastStart :: UTCTime
                         , lastMessg :: String }

initCommon :: FilePath -> FilePath -> FilePath -> Int -> IO CommonStruct
initCommon c u db i = do putStrLn "Initializing."
                         sv' <- newEmptyMVar
                         cu' <- connectSqlite3 u
                         cdb' <- connectSqlite3 db
                         s <- prepare cu' "SELECT f,id FROM unigrams"
                         uids' <- execStateT (cacheHash s) M.empty
                         disconnect cu'
                         putStrLn "Done."
                         t <- terminal_handle
                         return $ CommonStruct c uids' cdb' i sv' t

data SQL = RecordSQL { updateTarget  :: Statement
                     , insertContext :: Statement
                     , insertTarget  :: Statement }

indexItem :: UnigramIDs -> Item Text -> Item Int
indexItem udb i = (fromMaybe 1 . (`M.lookup` udb)) `fmap` i

countChunksI' :: Chan Int -> I.Iteratee ByteString (ReaderT CommonStruct IO) ()
countChunksI' log = I.liftI (step 0)
    where step (!i) (Chunk _) = let i' = i+1
                                in liftIO (writeChan log i') >> I.liftI (step i')
          step _    stream    = I.idone () stream

handle :: PFEGMain -> IO ()
handle (Record c u db _sql ts i) =
    bracket (do session <- initCommon c u db i
                hide_cursor (cTerm session)
                return session)
            (\session -> do
                putStrLn "Disconnecting…"
                disconnect (cDatabase session)
                show_cursor (cTerm session))
            (\session -> do
                insertCtxtS <- prepare (cDatabase session) insertCtxtSQL
                insertTrgtS <- prepare (cDatabase session) insertTargetSQL
                updateS     <- prepare (cDatabase session) updateSQL

                logVar <- newChan
                t0     <- getCurrentTime
                csize  <- withFile (cCorpus session) ReadMode hFileSize

                void $ forkIO $ logger ((fromIntegral csize `div` chunk_size)+1) t0 logVar
                let sql = RecordSQL { updateTarget  = updateS
                                    , insertContext = insertCtxtS
                                    , insertTarget  = insertTrgtS }
                    targets = map (T.strip.T.pack) $ splitOn "," ts

                runReaderT (I.run =<< enumFile chunk_size (cCorpus session) (I.sequence_
                    [ countChunksI' logVar
                    , I.joinI $ I.convStream corpusI (mainI (recordF sql) targets)])) session

                putStrLn "Committing…"
                doTimed_ (commit $ cDatabase session) >>= putStrLn.("Took "++).renderSecs.round
                putStrLn "Done.")

handle (Match  c u db _sql _ts i _r) = do
    _cs <- initCommon c u db i
    return ()

mainI :: (Item Text -> ReaderT CommonStruct IO ()) -> [Text]
        -> Iteratee (Sentence Text) (ReaderT CommonStruct IO) ()
mainI f targets = I.mapChunksM_ $ mapM f.getItems targets

recordF :: SQL -> Item Text -> ReaderT CommonStruct IO ()
recordF sql i = do cf <- ask
                   let pattern  = item2SQL $ indexItem (cUnigramIds cf) i
                       pattern' = toSql (cShard cf):toSql (target i):pattern
                   numRows <- liftIO $ execute (updateTarget sql) pattern'
                   when (numRows == 0) (do
                        void.liftIO $ execute (insertContext sql) pattern
                        void.liftIO $ execute (insertTarget  sql) pattern')

matchF :: SQL -> Item Text -> ReaderT CommonStruct IO ()
matchF = undefined

{-
statusLog :: MVar LogMessage -> IO ()
statusLog lv = do
    t0 <- getCurrentTime
    ls <- newEmptyMVar
    void $ forkIO $ void $ evalStateT (forever $ l' ls) (LogState t0 "Initializing.")
    where l :: MVar LogState -> IO ()
          l ls = do curM <- takeMVar lv
                    t <- getCurrentTime
                    case curM of
                         Start m    -> ls `putMVar` LogState t ("\nStart: " ++ m)
                         Done       -> ls `putMVar` LogState t "\nDone."
                         Message m  -> ls `putMVar` LogState t m
                         Progress p -> undefined
                         Finish     -> undefined
          l' :: MVar LogState -> StateT LogState IO ()
          l' ls = do maybeCurS <- liftIO $ tryTakeMVar ls
                     case maybeCurS of
                          Nothing -> do
                              t' <- liftIO getCurrentTime
                              (LogState t m) <- get
                              let s = renderSecs.round $ t' `diffUTCTime` t
                              liftIO $ putStr ("\r" ++ m ++ " (" ++ s ++ ")") >> threadDelay 300000
                          Just curS@(LogState _t m) -> liftIO (putStr m) >> put curS
-}


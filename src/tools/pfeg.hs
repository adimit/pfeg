{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import PFEG.SQL
import PFEG.Types
import PFEG.Common
import PFEG.Context

import System.Time.Utils (renderSecs)

import qualified Data.Iteratee as I
import Data.Iteratee (Iteratee)
import Data.Iteratee.IO

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
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void,forever)

import Database.HDBC
import Database.HDBC.Sqlite3

import System.Console.CmdArgs

import Graphics.Vty.Terminal

{- START CMDARGS GIBBERISH -}

data PFEGMain = Record { corpus    :: FilePath
                       , unigrams  :: FilePath
                       , database  :: FilePath
                       , sqlLog    :: Maybe FilePath
                       , shard     :: Int }
              | Match  { corpus    :: FilePath
                       , unigrams  :: FilePath
                       , database  :: FilePath
                       , sqlLog    :: Maybe FilePath
                       , shard     :: Int
                       , resultLog :: FilePath }
              deriving (Data, Typeable)

instance Show PFEGMain where
    show (Record c u db sql i)  = standardMessage "Recording to" c u db sql i ""
    show (Match  c u db sql i r) = standardMessage "Matching aigainst" c u db sql i ("Logging result to '" ++ r ++ "'")

standardMessage :: String -> FilePath -> FilePath -> FilePath -> Maybe FilePath -> Int -> String -> String
standardMessage m c u db sql i r = m ++ " '" ++ db ++ "'\n" ++
                                   "from corpus '" ++ c ++ "', shard " ++ show i ++ ".\n" ++
                                   "unigrams are '" ++ u ++ "'\n" ++ r ++ "\n" ++
                                   case sql of
                                        Nothing  -> ""
                                        (Just x) -> "Logging SQL to '" ++ x ++ "'.\n"

recordCmd, matchCmd :: PFEGMain
recordCmd = Record { corpus    = commonCorpus def
                   , unigrams  = commonUnigrams "../db/de/uni.db"
                   , database  = commonDatabase "../db/de/ctx.db"
                   , sqlLog    = commonSqlLog def
                   , shard     = commonShard 1 }
                              &= help "Learn contexts from corpus and store in DB."

matchCmd  = Match  { corpus    = commonCorpus def
                   , unigrams  = commonUnigrams "../db/de/uni.db"
                   , database  = commonDatabase "../db/de/ctx.db"
                   , sqlLog    = commonSqlLog def
                   , shard     = commonShard def
                   , resultLog = "result.log" &= typ "FILE" &= help "Result log file location."}
                              &= help "Match context from corpus against DB."

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

type UnigramIDs = HashMap Text Int

cacheHash :: Statement -> StateT UnigramIDs IO ()
cacheHash s = liftIO (void $ execute s []) >> fetchAll
    where fetchAll = do
          row <- liftIO $ fetchRow s
          case row of
               Just (f:i:[]) -> get >>= put . M.insert (fromSql f) (fromSql i) >> fetchAll
               Nothing       -> return ()
               _             -> fail "Malformed result in unigrams."

data CommonStruct = CommonStruct
    { cCorpus :: FilePath
    , cUnigramIds :: UnigramIDs
    , cDatabase :: Connection
    , cShard :: Int
    , cStatusVar :: MVar LogMessage
    , cTerm :: TerminalHandle }

data LogMessage = Done
                | Start { message :: String }
                | Message { message :: String }
                | Progress { progress :: Maybe Double }
                | Finish

data LogState = LogState { lastStart :: UTCTime
                         , lastMessg :: String }

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

recordI :: CommonStruct -> Statement -> Statement -> Statement -> Iteratee (Sentence Text) IO ()
recordI = undefined

indexItem :: Item Text (Context Text) -> Item Int (Context Int)
indexItem = undefined

handle :: PFEGMain -> IO ()
handle (Record c u db _sql i) =
    bracket (do session <- initCommon c u db i
                hide_cursor (cTerm session)
                return session)
            (\session -> do
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

                I.run =<< enumFile chunk_size (cCorpus session) (I.sequence_
                    [ countChunksI logVar
                    , I.joinI $ I.convStream corpusI (recordI session insertCtxtS insertTrgtS updateS)])

                putStrLn "Committing…"
                doTimed_ (commit $ cDatabase session) >>= putStrLn.("Took "++).renderSecs.round
                putStrLn "Done.")

handle (Match  c u db _sql i r) = do cs <- initCommon c u db i
                                     return ()

{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

import Control.Monad.Trans.State.Strict

import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

import Database.HDBC
import Database.HDBC.Sqlite3

import Data.Text (Text)
import System.Console.CmdArgs

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

data CommonStruct = CommonStruct { cc :: FilePath, uids :: UnigramIDs, cdb :: Connection, ci :: Int }

initCommon :: FilePath -> FilePath -> FilePath -> Int -> IO CommonStruct
initCommon c u db i = do cu' <- connectSqlite3 u
                         cdb' <- connectSqlite3 db
                         s <- prepare cu' "SELECT f,id FROM unigrams"
                         uids' <- execStateT (cacheHash s) M.empty
                         disconnect cu'
                         return $ CommonStruct c uids' cdb' i

handle :: PFEGMain -> IO ()
handle (Record c u db sql i)   = do cs <- initCommon c u db i
                                    return ()
handle (Match  c u db sql i r) = do cs <- initCommon c u db i
                                    return ()

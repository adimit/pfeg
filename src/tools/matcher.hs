{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

import Control.Monad.Trans.State.Strict

import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

import Database.HDBC

import Data.Text (Text)
import System.Console.CmdArgs

data PFEFMain = Record { corpus    :: FilePath
                       , unigrams  :: FilePath
                       , database  :: FilePath
                       , shard     :: Int }
              | Match  { corpus    :: FilePath
                       , unigrams  :: FilePath
                       , database  :: FilePath
                       , shard     :: Int
                       , sqlLog    :: FilePath
                       , resultLog :: FilePath }
              deriving (Show, Data, Typeable)
-- def &= opt "report.html" &= typFile &= help "Generate a report in HTML"
recordCmd = Record { corpus    = def &= typFile   &= help "Target corpus in TT format to learn samples from."
                   , unigrams  = def &= typFile   &= help "Location of the unigram DB."
                   , database  = def &= typFile   &= help "Location of the DB to record to."
                   , shard     = def &= typ "INT" &= help "The index of the shard we're recording from." }

matchCmd  = Match  { corpus    = def &= typFile   &= help "Target corpus in TT format to match samples from."
                   , unigrams  = def &= typFile   &= help "Location of the unigram DB."
                   , database  = def &= typFile   &= help "Location of the DB holding the sample contexts."
                   , sqlLog    = def &= typFile   &= help "SQL log file location."
                   , resultLog = def &= typFile   &= help "Result log file location."
                   , shard     = def &= typ "INT" &= help "Shard index to ignore when matching data against." }


cacheHash :: Statement -> StateT (M.HashMap Text Int) IO ()
cacheHash s = liftIO (void $ execute s []) >> fetchAll
    where fetchAll = do
          row <- liftIO $ fetchRow s
          case row of
               Just (f:i:[]) -> get >>= put . M.insert (fromSql f) (fromSql i) >> fetchAll
               Nothing       -> return ()
               _             -> fail "Malformed result in unigrams."

main :: IO ()
main = undefined

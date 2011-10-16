{-# LANGUAGE TupleSections,BangPatterns #-}
module Main where

import Prelude hiding (log)

import PFEG.Types
import PFEG.Common
import PFEG.SQL
import PFEG.Context
import qualified PFEG.BinaryMagic as Magic -- it's a kind of magic!

import System.Environment (getArgs)
import System.IO (hClose,openFile,hFileSize,withFile,IOMode(ReadMode,WriteMode))
import System.Time.Utils

import Database.HDBC
import Database.HDBC.Sqlite3

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import Data.Attoparsec.Iteratee
import Data.Iteratee.IO
import Data.Iteratee.Base
import qualified Data.Iteratee as I

import Data.Time.Clock
import Data.List (findIndices)
import Data.Maybe (fromMaybe)
import Data.Int (Int32)
import Data.Functor ((<$>))
import Safe (atMay)

import Control.Monad (liftM,forever,void,when,(>=>))
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Exception (bracket)

import Graphics.Vty.Terminal

import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)

data MatchMode = POS | LEM | CRD | SFC | NIL
type Match = Context MatchMode
type Count = Int
data Result = Correct Match Count -- ^ Found a match, and the prediction was correct.
            | Baseline Bool -- ^ Didn't find a match. Choose baseline and report if correct.
            | Wrong Match Count -- ^ Found a match, but the prediction was incorrect.

matchContext :: Statement -> -- ^ SQL lookup statement for unigram indices
               DBStatements -> -- ^ SQL statements for looking up cP cL cS and cC values
               Item Text (Context (Bracket Text)) -> -- ^ An Item to match
               IO Result
matchContext lup stmts i = do
    iBS <- liftM ((fmap.fmap) Magic.encodePair) (indexItem lup i)
    
    -- check for s1 s2 s3
    -- s2id <- check for s1 s2
    -- when s2id check for s1 s2 l3
    -- check for s1


    return undefined

main :: IO ()
main = do
    (unigramT:contextT:corpus:[]) <- getArgs

    startTime <- getCurrentTime
    term      <- terminal_handle
    csize     <- withFile corpus ReadMode hFileSize

    let logname = corpus ++ ".log"
    logfile   <- openFile logname WriteMode
    putStrLn $ "Logging results to "++logname

    bracket (do putStr "Connecting…"
                unigramA  <- establishConnection (unigramTable standardConfig) unigramT
                contextdb <- connectSqlite3 contextT
                hide_cursor term
                return (unigramA,contextdb))
            (\(unigramA,contextdb) ->
             do show_cursor term
                putStrLn "Disconnecting…"
                disconnect contextdb
                hClose logfile
                disconnect $ connection unigramA)
            (\(unigramA,contextdb) ->
             do undefined)

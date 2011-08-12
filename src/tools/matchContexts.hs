module Main where

import PFEG.Types
import PFEG.Common
import PFEG.SQL
import PFEG.BinaryMagic
import System.Environment (getArgs)

import Database.HDBC
import Database.HDBC.Sqlite3

import Data.ByteString (ByteString)

import Data.Iteratee.IO
import qualified Data.Iteratee as I
import Data.Iteratee.Parallel as IP

import Data.Attoparsec.Iteratee

import qualified Data.Text as T
import Data.Text (Text)

import Data.List (findIndices)

import Data.Int (Int32)

import Control.Monad ((>=>))
import Safe (atMay)


main :: IO ()
main = do
    (unigramT:contextT:corpus:[]) <- getArgs

    unigramA <- establishConnection (unigramTable standardConfig) unigramT
    contextA <- establishConnection (contextTable standardConfig) contextT

    return ()

module Main where

import PFEG.Types
import PFEG.Common
import System.Environment (getArgs)

import Database.HDBC
import Database.HDBC.Sqlite3

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.ByteString (ByteString)

import Data.Iteratee.Char
import Data.Iteratee.IO
import Data.Iteratee.ListLike as LL
import qualified Data.Iteratee as I

import Data.Attoparsec
import Data.Attoparsec.Iteratee

import Data.Word (Word8)

import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Control.Applicative hiding (many)

establishConnections :: FilePath -> FilePath -> IO (TableAccess,TableAccess)
establishConnections unigramT contextT = do
    unigramC <- connectSqlite3 unigramT
    contextC <- connectSqlite3 contextT
    let unigramA = Access { connection = unigramC
                          , table = unigramTable standardConfig }
        contextA = Access { connection = contextC
                          , table = "contexts" }
    return (unigramA,contextA)

corpusI :: (Monad m) => Iteratee ByteString m [Sentence Text]
corpusI = parserToIteratee (many sentenceP)
{-# INLINE corpusI #-}

extractItems :: [Sentence Text] -> [Item Text Text]
extractItems (s:ss) = exS s ++ extractItems ss
    where exS s' = undefined

main :: IO ()
main = do
    (unigramT:contextT:corpus:_) <- getArgs
    (unigramA,contextA) <- establishConnections unigramT contextT
    return ()

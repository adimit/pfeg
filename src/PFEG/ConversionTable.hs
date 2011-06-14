module PFEG.ConversionTable where

import Database.HDBC
import Database.HDBC.Sqlite3

import PFEG.BinaryMagic
import PFEG.Common
import PFEG.Types

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy as S

import Data.Int

-- Convenient IO wrappers around SQL statements.

sqlIndex2Form, sqlForm2Index :: String
sqlIndex2Form = "SELECT form FROM " ++ (unigramTable standardConfig) ++ " WHERE id==?"
sqlForm2Index = "SELECT id FROM " ++ (unigramTable standardConfig) ++ " WHERE form==?"

convertValue :: String -> Connection -> SqlValue -> IO SqlValue
convertValue sql conn = fmap (head.concat).quickQuery conn sql . (:[])

indexContext :: Connection -> Context S.ByteString -> IO (Context Int32)
indexContext f c = case c of 
    (Context3 a1 a2 a3 a4 a5 a6) -> undefined

unindexContext :: Connection -> Context Int32 -> IO (Context L.ByteString)
unindexContext f c = case c of
    (Context3 a1 a2 a3 a4 a5 a6) -> undefined-}

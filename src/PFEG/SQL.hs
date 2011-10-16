{-# LANGUAGE FlexibleContexts #-}
module PFEG.SQL
    ( -- * Utility SQLite3 function
      establishConnection 
      -- * Wrappers around SQL Statements
    , lookupIndexSQL
    , Statements(..)
    , getcPStatements
    , getc'Statements
    , DBStatements(..)
      -- * Marshalling PFEG types to/from SQL values
    , context2SQL
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3

import PFEG.Types
import PFEG.Context
import PFEG.Common

import qualified Data.ByteString.Lazy as L
import Data.List (intercalate,intersperse)
import Data.Text (Text)

import Data.Convertible.Base (Convertible)

{-|
 - Database creation:
 -
 - sqlite> .schema
 - CREATE TABLE cL (id integer primary key autoincrement,
 - c1 integer, c2 integer, c3 integer, c4 integer, c5 integer, c6 integer,
 - T text, count integer, ref integer,
 - UNIQUE(c1,c2,c3,c4,c5,c6,T,ref));
 - CREATE TABLE cP (id integer primary key autoincrement,
 - c1 integer, c2 integer, c3 integer, c4 integer, c5 integer, c6 integer,
 - T text, count integer,
 - UNIQUE(c1,c2,c3,c4,c5,c6,T));
 - CREATE TABLE cS (id integer primary key autoincrement,
 - c1 integer, c2 integer, c3 integer, c4 integer, c5 integer, c6 integer,
 - T text, count integer, ref integer,
 - UNIQUE(c1,c2,c3,c4,c5,c6,T,ref));
 -
 -}

-- | Create a @TableAccess@ data structure from a @String@, denotating the table
-- name within the db file, and a @FilePath@ for the db file
establishConnection :: String -> FilePath -> IO TableAccess
establishConnection tn fp = do
    conn <- connectSqlite3 fp
    return $ Access { connection = conn , table = tn }

lookupIndexSQL :: TableAccess -> IO Statement
lookupIndexSQL acc =
    prepare (connection acc)
            ("SELECT id FROM " ++ table acc ++ " WHERE form == ?")

-- Make "ca==? AND … AND cb ==?" for given range [a…b]
cConjunction :: [Int] -> String
cConjunction = intercalate " AND ".map (++"==?").cLetters

-- Make "ca,…,cb" for given range [a…b]
cCommas :: [Int] -> String
cCommas = intercalate ",".cLetters

-- Make ["ca"…"cb"] for given range [a…b]
cLetters :: [Int] -> [String]
cLetters = map (('c':) . show)

questionmarks :: [Int] -> String
questionmarks range = intersperse ',' $ replicate (length range) '?'

lookupIDStmtString  range tn = "SELECT id FROM " ++ tn ++
    " WHERE            T==? AND " ++ cConjunction range
lookupIDStmtString' range tn = "SELECT id FROM " ++ tn ++
    " WHERE ref==? AND T==? AND " ++ cConjunction range
updateStmtString  range tn = "UPDATE " ++ tn ++ " SET count=count+1 " ++
    " WHERE            T==? AND " ++ cConjunction range
updateStmtString' range tn = "UPDATE " ++ tn ++ " SET count=count+1 " ++
    " WHERE ref==? AND T==? AND " ++ cConjunction range
insertStmtString  range tn =
    "INSERT INTO "++tn++" (    T,"++cCommas range++") VALUES (  ?,"++questionmarks range++")"
insertStmtString' range tn =
    "INSERT INTO "++tn++" (ref,T,"++cCommas range++") VALUES (?,?,"++questionmarks range++")"

data Statements = Statements { insertStatement :: Statement
                             , updateStatement :: Statement
                             , lookupStatement :: Statement }
type DBStatements = (Statements,Statements,Statements)

getcPStatements :: TableAccess -> IO Statements
getcPStatements acc = do
    ins <- prepare (connection acc)
                   (insertStmtString [1..6] $ table acc)
    lup <- prepare (connection acc)
                   (lookupIDStmtString [1..6] $ table acc)
    upd <- prepare (connection acc)
                   (updateStmtString [1..6] $ table acc)
    return $ Statements ins upd lup

getc'Statements :: TableAccess -> IO Statements
getc'Statements acc = do
    ins <- prepare (connection acc)
                   (insertStmtString' [1..6] $ table acc)
    lup <- prepare (connection acc)
                   (lookupIDStmtString' [1..6] $ table acc)
    upd <- prepare (connection acc)
                   (updateStmtString' [1..6] $ table acc)
    return $ Statements ins upd lup

context2SQL :: (Convertible a SqlValue) => Context a -> Target -> [SqlValue]
context2SQL (Context xs) t = toSql t:map toSql xs

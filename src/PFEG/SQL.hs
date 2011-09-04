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
import Data.List (intercalate)
import Data.Text (Text)

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

lookupIDStmtString  tn = "SELECT id FROM " ++ tn ++
    " WHERE            T==? AND c1==? AND c2==? AND c3==?"
lookupIDStmtString' tn = "SELECT id FROM " ++ tn ++
    " WHERE ref==? AND T==? AND c1==? AND c2==? AND c3==?"
updateStmtString  tn = "UPDATE " ++ tn ++ " SET count=count+1 " ++
    " WHERE            T==? AND c1==? AND c2==? AND c3==?"
updateStmtString' tn = "UPDATE " ++ tn ++ " SET count=count+1 " ++
    " WHERE ref==? AND T==? AND c1==? AND c2==? AND c3==?"
insertStmtString  tn = "INSERT INTO " ++ tn ++ " (    T,c1,c2,c3) VALUES (  ?,?,?,?)"
insertStmtString' tn = "INSERT INTO " ++ tn ++ " (ref,T,c1,c2,c3) VALUES (?,?,?,?,?)"

data Statements = Statements { insertStatement :: Statement
                             , updateStatement :: Statement
                             , lookupStatement :: Statement }
type DBStatements = (Statements,Statements,Statements,Statements)

getcPStatements :: TableAccess -> IO Statements
getcPStatements acc = do
    ins <- prepare (connection acc)
                   (insertStmtString $ table acc)
    lup <- prepare (connection acc)
                   (lookupIDStmtString $ table acc)
    upd <- prepare (connection acc)
                   (updateStmtString $ table acc)
    return $ Statements ins upd lup

getc'Statements :: TableAccess -> IO Statements
getc'Statements acc = do
    ins <- prepare (connection acc)
                   (insertStmtString' $ table acc)
    lup <- prepare (connection acc)
                   (lookupIDStmtString' $ table acc)
    upd <- prepare (connection acc)
                   (updateStmtString' $ table acc)
    return $ Statements ins upd lup

context2SQL :: Context L.ByteString -> Target -> [SqlValue]
context2SQL (Context (a,b,c)) t = toSql t:map toSql [a,b,c]

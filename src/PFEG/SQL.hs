module PFEG.SQL where

import Database.HDBC
import PFEG.Types
import PFEG.Common

import qualified Data.ByteString.Lazy as L
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

recordContextSQL :: TableAccess -> IO Statement
recordContextSQL acc =
    prepare (connection acc)
            ("INSERT OR REPLACE INTO "++ contextTable standardConfig
            ++" (context,target,count) VALUES (?,?,COALESCE((SELECT count FROM "
            ++ contextTable standardConfig
            ++" WHERE context == ? AND target == ?)+1,1))")

recordContextValues :: L.ByteString -> Text -> [SqlValue]
recordContextValues context target =
    let x = [toSql context, toSql target] in x++x

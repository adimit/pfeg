module PFEG.SQL where

import Database.HDBC
import PFEG.Types
import PFEG.Common

import Data.ByteString.Lazy as L
import Data.Text (Text)

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

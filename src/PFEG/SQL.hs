module PFEG.SQL
    ( insertAction
    , upsertRecord
    , item2SQL
    ) where

import Database.HDBC

import PFEG.Types
import PFEG.Context

import Data.Text (Text)
import qualified Data.Text as T

import Paths_PFEGtools (version)
import Data.Version (showVersion)

insertAction :: String
insertAction = "INSERT INTO log (action,corpusname,corpusfile,completed,version) VALUES (?,?,?,?,'"
             ++ showVersion version ++ "')"

upsertRecord :: String
upsertRecord = "INSERT INTO records (lcs,rcs,lcl,rcl,target) VALUES (?,?,?,?,?)"

item2SQL :: Item Text -> [SqlValue]
item2SQL Item { itemLemma = (Context ll rl) , itemSurface = (Context ls rs), target = t } =
    [ tsql ls, tsql rs, tsql ll, tsql rl, toSql (surface t) ]
    where tsql = toSql . T.unwords

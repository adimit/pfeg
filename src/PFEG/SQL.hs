module PFEG.SQL
    ( insertAction
    , upsertRecord
    , item2SQL
    ) where

import Database.HDBC

import PFEG.Context

import Data.Text (Text)
import qualified Data.Text as T

import Paths_PFEGtools (version)
import Data.Version (showVersion)

import Codec.Digest.SHA

insertAction :: String
insertAction = "INSERT INTO log (action,corpusname,corpusfile,completed,version) VALUES (?,?,?,?,'"
             ++ showVersion version ++ "')"

upsertRecord :: Int -> String
upsertRecord tnum = "INSERT INTO records (hash,lcs,rcs,lcl,rcl,"++targetSql++") \
               \VALUES (UNHEX(?),?,?,?,?,1) ON DUPLICATE KEY UPDATE "
               ++targetSql++" = COALESCE("++targetSql++",0) + 1"
          where targetSql = 't':show tnum


item2SQL :: Item Text -> [SqlValue]
item2SQL item@Item { itemLemma = (Context ll rl) , itemSurface = (Context ls rs) } =
    [ toSql . showBSasHex $ hash SHA256 item
    , tsql ll, tsql rl, tsql ls, tsql rs ]
    where tsql = toSql . T.unwords

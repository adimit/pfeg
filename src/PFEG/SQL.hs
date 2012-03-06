module PFEG.SQL
    ( insertAction
    , upsertRecord
    , item2SQL
    ) where

import Database.HDBC

import PFEG.Context

import PFEG

import Data.Text (Text)
import qualified Data.Text as T

import Paths_PFEGtools (version)
import Data.Version (showVersion)

import Codec.Digest.SHA

insertAction :: String
insertAction = "INSERT INTO log (action,corpusname,corpusfile,completed,version) VALUES (?,?,?,?,'"
             ++ showVersion version ++ "')"

upsertRecord :: String
upsertRecord = "INSERT INTO records (hash,lcs,rcs,lcl,rcl,?) \
               \VALUES (UNHEX(?),?,?,?,?,1) ON DUPLICATE KEY UPDATE ? = COALESCE(?,0) + 1"

item2SQL' :: Int -> Item Text -> [SqlValue]
item2SQL' tnum item@Item { itemLemma = (Context ll rl) , itemSurface = (Context ls rs) } =
    [ targetSql
    , toSql . showBSasHex $ hash SHA256 item
    , tsql ll, tsql rl, tsql ls, tsql rs
    , targetSql, targetSql ]
    where tsql = toSql . T.unwords
          targetSql = toSql $ 't':show tnum

item2SQL :: Item Text -> PFEG a [SqlValue]
item2SQL item@Item { target = t } = do
    tnum <- targetNo t
    return $ item2SQL' tnum item
{-# INLINE item2SQL #-}

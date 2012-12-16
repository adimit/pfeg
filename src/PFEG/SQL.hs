module PFEG.SQL
    ( insertAction
    , upsertRecord
    , sentence2SQL
    , insertText
    , document2SQL
    ) where

import Database.HDBC

import PFEG.Types

import Data.Text (Text)
import qualified Data.Text as T

import Paths_PFEGtools (version)
import Data.Version (showVersion)

insertAction :: String
insertAction = "INSERT INTO log (action,corpusname,corpusfile,completed,version) VALUES (?,?,?,?,'"
             ++ showVersion version ++ "')"

upsertRecord :: String
upsertRecord = "INSERT INTO records (lcs,rcs,lcl,rcl,target) VALUES (?,?,?,?,?)"

insertText :: String
insertText = "INSERT INTO records (surface,lemma) VALUES (?,?)"

-- | Prepare a @Sentence Text@ for use with @insertText@.
sentence2SQL :: Sentence Text -> [SqlValue]
sentence2SQL s = [sqlify surface, sqlify lemma]
    where sqlify x = toSql . T.unwords . fmap x $ s

-- | Prepare a @Document Text@ for use with @insertText@
document2SQL :: Document Text -> [SqlValue]
document2SQL t = [sqlify surface, sqlify lemma]
    where sqlify x = toSql . T.unwords . map (T.unwords . fmap x) $ t


module PFEG.SQL
    ( insertAction
    , upsertRecord
    , sentence2SQL
    , insertSentence
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

insertSentence :: String
insertSentence = "INSERT INTO records (surface,lemma,pos) VALUES (?,?,?)"

-- | Prepare a @Sentence Text@ for use with @insertSentence@.
sentence2SQL :: Sentence Text -> [SqlValue]
sentence2SQL s = [sqlify surface, sqlify lemma, sqlify pos]
    where sqlify x = toSql . T.unwords . fmap x $ s

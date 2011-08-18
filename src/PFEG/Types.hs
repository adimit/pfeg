module PFEG.Types
    ( Wordcounts
    , Word
    , Sentence
    , Configuration(..)
    , Table
    , TableAccess(..)
    ) where

import Data.Int
import qualified Data.ByteString as S
import qualified Data.Text as X
import qualified Data.HashMap.Strict as T
import Database.HDBC
import Database.HDBC.Sqlite3

data TableAccess = Access { connection :: Connection, table :: Table }

type Wordcounts = T.HashMap X.Text Int
type Word a = (a,a,a)
type Sentence a = [Word a]

data Configuration = Config { lemmaTable   :: String
                            , posTable     :: String
                            , contextTable :: String
                            , unigramTable :: String
                            , targets      :: [String] }

type Table = String

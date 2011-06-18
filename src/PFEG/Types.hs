module PFEG.Types
    ( Wordcounts
    , Word
    , Configuration(..)
    , Context(..)
    , TextContext
    , IdxContext
    , Table
    , TableAccess(..)
    , Item(..)
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

data Configuration = Config { lemmaTable   :: String
                            , posTable     :: String
                            , unigramTable :: String
                            , targets      :: [String] }

type TextContext = Context S.ByteString
type IdxContext  = Context Int32

type Table = String

data Context a = Context3 a a a a a a
               | Context2   a a a a
               | Context1     a a
               deriving (Show)

data Item a i = Item { target :: i, context :: a }

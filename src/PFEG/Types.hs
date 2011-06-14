module PFEG.Types
    ( Wordcounts
    , Configuration(..)
    , Context(..)
    , TextContext
    , IdxContext
    , Table
    ) where

import Data.Int
import qualified Data.ByteString as S
import qualified Data.Text as X
import qualified Data.HashMap.Strict as T

type Wordcounts = T.HashMap X.Text Int
data Configuration = Config { unigramTable :: String }

type TextContext = Context S.ByteString
type IdxContext  = Context Int32

type Table = String

data Context a = Context3 a a a a a a
               | Context2   a a a a
               | Context1     a a
               deriving (Show)

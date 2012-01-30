module PFEG.Types
    ( Wordcounts
    , Word
    , Sentence
    , Table
    , ID
    ) where

import qualified Data.Text as X
import qualified Data.HashMap.Strict as T

type Wordcounts = T.HashMap X.Text Int
type Word a = (a,a,a)
type Sentence a = [Word a]

type Table = String

type ID = Int -- SQL row IDs

module PFEG.Types
    ( Wordcounts
    , Word
    , Sentence
    ) where

import qualified Data.Text as X
import qualified Data.HashMap.Strict as T
import Data.Bits
import Data.Hashable
import Data.List (foldl')

type Wordcounts = T.HashMap X.Text Int
type Word a = (a,a,a)
type Sentence a = [Word a]

data MatchMode = P | L | S deriving (Show,Eq)
newtype MatchPattern = MatchPattern { unMatchPattern :: [Maybe MatchMode] } deriving (Eq)

instance Show MatchPattern where
    show = Prelude.take 6.concatMap (maybe "_" show).(++ repeat Nothing).unMatchPattern

instance Hashable MatchPattern where
    hash = foldl' f 1.unMatchPattern
       where f x (Nothing) = shift x 2
             f x (Just P ) = shift x 2 .|. 1
             f x (Just L ) = shift x 2 .|. 2
             f x (Just S ) = shift x 2 .|. 3

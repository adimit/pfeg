{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable  #-}
module PFEG.Context
    ( -- * Types
      Context(..)
    , Item(..)
      -- * Transformation functions
    , getItems
    ) where

import PFEG.Common
import PFEG.Types

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Traversable as Tr

import Database.HDBC

import Data.List (findIndices)
import Data.Maybe (fromMaybe)

import Data.Traversable (Traversable)
import Data.Foldable (Foldable)

import Safe (atMay)

newtype Context a = Context [a] deriving (Eq,Functor,Foldable,Traversable)

data Item i a = Item { pItem :: a -- ^ Part of speech part of the item
                     , lItem :: a -- ^ Lemma part of the item
                     , sItem :: a -- ^ Surface part of the item
                     , target :: i-- ^ Target functional element of the item
                     } deriving (Functor,Foldable,Traversable)

getItems :: Sentence Text -> [Item Text (Context Text)]
getItems s = let target_indices = findIndices (\(w,_,_) -> w `elem` targets') s
             in  map (getItem s) target_indices

getItem :: Sentence Text -> Int -> Item Text (Context Text)
getItem s i = let nt          = T.pack "NULL" -- Special null-unigram
                  wordContext = Context [a,b,c,d,e,f]
                  (a:b:c:t:d:e:f:[]) = map (fromMaybe (nt,nt,nt).atMay s) [i-3..i+3]
              in  Item { pItem = fmap trd3  wordContext
                       , lItem = fmap snd3  wordContext
                       , sItem = fmap fst3  wordContext
                       , target = fst3 t }

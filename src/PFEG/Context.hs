{-# LANGUAGE FlexibleInstances, DeriveFunctor, DeriveFoldable, DeriveTraversable  #-}
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

import Data.List (findIndices)
import Data.Maybe (fromMaybe)

import Data.Traversable (Traversable)
import Data.Foldable (Foldable)

import Safe (atMay)

import Prelude hiding (null)

newtype Context a = Context [a] deriving (Eq,Functor,Foldable,Traversable,Show)

data Item i = Item { pItem :: Context i -- ^ Part of speech part of the item
                   , lItem :: Context i -- ^ Lemma part of the item
                   , sItem :: Context i -- ^ Surface part of the item
                   , target :: i-- ^ Target functional element of the item
                   } deriving (Functor,Foldable,Traversable,Show)

-- | Get all items in a text
getItems :: Sentence Text -> [Item Text]
getItems s = let s_no_punct = filter (\(_,_,p) -> T.head p /= '$') s -- punctuation is tagged starting with $.
                 target_indices = findIndices (\(w,_,_) -> w `elem` targets') s_no_punct
             in  map (getItem s_no_punct) target_indices

-- | Get the item in sentence @s@ at position @i@.
getItem :: Sentence Text -> Int -> Item Text
getItem s i = let nt          = T.pack "NULL" -- Special null-unigram
                  wordContext = Context [a,b,c,d,e,f]
                  (a:b:c:t:d:e:f:[]) = map (fromMaybe (nt,nt,nt).atMay s) [i-3..i+3]
              in  Item { lItem = fmap trd3  wordContext
                       , pItem = fmap snd3  wordContext
                       , sItem = fmap fst3  wordContext
                       , target = fst3 t }

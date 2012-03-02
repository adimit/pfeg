{-# LANGUAGE FlexibleInstances, DeriveFunctor, DeriveFoldable, DeriveTraversable  #-}
module PFEG.Context
    ( -- * Types
      Context(..)
    , Item(..)
      -- * Transformation functions
    , getItems
    ) where

import PFEG.Types
import PFEG.Common

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding

import Data.List (findIndices)
import Data.Maybe (fromMaybe)

import Data.Traversable (Traversable)
import Data.Foldable (Foldable)

import Safe (atMay)

import Codec.Digest.SHA.Monad

newtype Context a = Context [a] deriving (Eq,Functor,Foldable,Traversable,Show)

data Item i = Item { pItem :: Context i -- ^ Part of speech part of the item
                   , lItem :: Context i -- ^ Lemma part of the item
                   , sItem :: Context i -- ^ Surface part of the item
                   , target :: i-- ^ Target functional element of the item
                   } deriving (Functor,Foldable,Traversable,Show)

-- | Hash an item without targets. I.e. items with different targets which are
-- otherwise the same will hash to the same value!
instance Hashable (Item Text) where
    update (Item { pItem = (Context pI), lItem = (Context lI), sItem = (Context sI) }) =
           update $ encodeUtf8 (T.intercalate (T.singleton '.') (pI++lI++sI))

-- | Get all items in a text
getItems :: [Text] -> Sentence Text -> [Item Text]
getItems t s = let target_indices = findIndices ((`elem` t).surface) s
               in  map (getItem s) target_indices

-- | Get the item in sentence @s@ at position @i@.
getItem :: Sentence Text -> Int -> Item Text
getItem s i = let wordContext = Context [a,b,c,d,e,f]
                  (a:b:c:t:d:e:f:[]) = map (fromMaybe (Word nullToken nullToken nullToken) . atMay s)
                                           [i-3..i+3]
              in  Item { lItem = fmap lemma  wordContext
                       , pItem = fmap pos  wordContext
                       , sItem = fmap surface  wordContext
                       , target = surface t }

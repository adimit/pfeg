{-# LANGUAGE FlexibleInstances, DeriveFunctor, DeriveFoldable, DeriveTraversable  #-}
module PFEG.Context
    ( -- * Types
      Context(..)
    , Item(..)
      -- * Transformation functions
    , getItems
    ) where

import PFEG.Types

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding

import Data.List (findIndices)

import Data.Traversable (Traversable)
import Data.Foldable (Foldable)

import Codec.Digest.SHA.Monad

data Context a = Context { left  :: ![a]
                         , right :: ![a]
                         } deriving (Functor,Show,Foldable,Traversable)

data Item a = Item { itemLemma   :: !(Context a)
                   , itemSurface :: !(Context a)
                   , target      :: !a
                   } deriving (Functor,Show,Foldable,Traversable)

getItems :: [Text] -> Sentence Text -> [Item Text]
getItems t s = let target_indices = findIndices ((`elem` t).surface) s
               in  map (getItem s) target_indices

getItem :: Sentence Text -> Int -> Item Text
getItem s i = let (l,t:r) = splitAt i s
                  cc = Context l r
              in Item { itemLemma = fmap lemma cc
                      , itemSurface = fmap surface cc
                      , target = surface t }

-- | Hash only the surface of an item, with an 'X' in between to keep apart [a,b] [] and [a] [b].
instance Hashable (Item Text) where
    update Item { itemSurface = (Context l r) } = update $ encodeUtf8
        (T.concat $ l ++ [T.singleton 'X'] ++ r)

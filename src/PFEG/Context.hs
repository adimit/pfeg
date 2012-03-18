{-# LANGUAGE FlexibleInstances, DeriveFunctor, DeriveFoldable, DeriveTraversable  #-}
module PFEG.Context
    ( -- * Types
      Context(..)
    , Item(..)
    , ItemGetter
      -- * Transformation functions
    , getItems
    , getMaskedItems
    , getMaskedItems'
    ) where

import PFEG.Types

import Control.Monad (liftM2)
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
                   , target      :: !(Token a)
                   } deriving (Functor,Show,Foldable,Traversable)

getMaskedItems :: ItemGetter
getMaskedItems s = map (getItem s) $ findIndices isMasked s

getMaskedItems' :: [Text] -> ItemGetter
getMaskedItems' ts s = map (getItem s) $ findIndices (liftM2 (&&) isMasked (isTarget ts)) s

type ItemGetter = Sentence Text -> [Item Text]

isMasked :: Token a -> Bool
isMasked Masked {} = True
isMasked _         = False

isTarget :: (Eq a) => [a] -> Token a -> Bool
isTarget ts t | surface t `elem` ts = True
              | otherwise           = False

getItems :: [Text] -> ItemGetter
getItems t s = let target_indices = findIndices ((`elem` t).surface) s
               in  map (getItem s) target_indices

getItem :: Sentence Text -> Int -> Item Text
getItem s i = let (l,t:r) = splitAt i s
                  cc = Context l r
              in Item { itemLemma = fmap lemma cc
                      , itemSurface = fmap surface cc
                      , target = t }

-- | Hash only the surface of an item, with an 'X' in between to keep apart [a,b] [] and [a] [b].
instance Hashable (Item Text) where
    update Item { itemSurface = (Context l r) } = update $ encodeUtf8
        (T.concat $ l ++ [T.singleton 'X'] ++ r)

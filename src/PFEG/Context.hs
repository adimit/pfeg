{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module PFEG.Context
    ( -- * Types
      Context(..)
    , Item
    , ItemGetter
    , Restriction
      -- * Transformation functions
    , restrictContext
    , getSentenceItems
    , getMaskedItems
    ) where

import PFEG.Types

import Data.Text (Text)

import Data.List (findIndices)

import Data.Traversable (Traversable)
import Data.Foldable (Foldable)

import Control.Monad.Reader

data Context a = Context { left  :: ![a]
                         , right :: ![a]
                         } deriving (Functor,Show,Foldable,Traversable)

type Item a = (Token a,Context (Token a))
type ItemGetter = Document Text -> [Item Text]

isMasked :: Token a -> Bool
isMasked Masked {} = True
isMasked _         = False

getContexts :: (a -> Bool) -> [a] -> [(a,Context a)]
getContexts p s = map (mkContext . \i -> splitAt i s) $ findIndices p s
                  where mkContext (_,[]) = error "findIndices returned some garbage!"
                        mkContext (a,b) = (head b,Context { left = a, right = tail b })

getSentenceItems :: (Text -> Bool) -> ItemGetter
getSentenceItems p = getContexts (p.surface) . concat

getMaskedItems :: (Text -> Bool) -> ItemGetter
getMaskedItems p = getContexts (liftM2 (&&) isMasked (p.surface)) . concat

type Restriction = (Int,Int)

restrictContext :: Restriction -> Context a -> Context a
restrictContext (i_l,i_r) Context { left = l, right = r } =
    Context { left = reverse . take i_l . reverse $ l, right = take i_r r }

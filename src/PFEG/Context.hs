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
      -- Misc
    , period -- a token representing a period
    ) where

import PFEG.Types

import Data.Text (Text)

import Data.List (findIndices)

import Data.Traversable (Traversable)
import Data.Foldable (Foldable)

period :: Token Text
period = Word { surface = ".", lemma = ".", pos = "$." }

data Context a = Context { left  :: ![a]
                         , right :: ![a]
                         } deriving (Functor,Show,Foldable,Traversable)

type Item a = (Token a,Context (Token a))
type ItemGetter = Document Text -> [Item Text]

getContexts :: (a -> Bool) -> [a] -> [(a,Context a)]
getContexts p s = map (mkContext . flip splitAt s) $ findIndices p s
                  where mkContext (_,[]) = error "findIndices returned some garbage!"
                        mkContext (a,b) = (head b,Context { left = a, right = tail b })

getSentenceItems :: (Text -> Bool) -> ItemGetter
getSentenceItems p = concatMap (getContexts (p.surface) . (period:) . filter (not.punctuation))
    where punctuation t = surface t `elem` [","]

type Restriction = (Int,Int)

restrictContext :: Restriction -> Context a -> Context a
restrictContext (i_l,i_r) Context { left = l, right = r } =
    Context { left = reverse . take i_l . reverse $ l, right = take i_r r }

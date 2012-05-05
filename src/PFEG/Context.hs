{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module PFEG.Context
    ( -- * Types
      Context(..)
    , Item(..)
    , ItemGetter
      -- * Transformation functions
    , getItems
    , getMaskedItems
    , getMaskedItems'
    , tagSentence
    ) where

import PFEG
import PFEG.Configuration
import PFEG.Types

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding

import Data.List (findIndices,mapAccumL)

import Data.Traversable (Traversable)
import Data.Foldable (Foldable)

import Control.Monad.Reader
import Codec.Digest.SHA.Monad

o_targetTag, c_targetTag :: Token Text
o_targetTag = nonWord "<t>"
c_targetTag = nonWord "</t>"

-- | Insert tags around items in a list at given indices. Indices are 0-based
-- and *must* be sorted.
-- CAREFUL: If the tagged @Token@s are anything but @Word@, they will not
-- retain that property, but they will be turned into @Word@ upon tagging, even
-- if they use, say @Masked@.
insertTags :: [Token Text] -- ^ The list to insert tags into
             -> (Token Text,Token Text) -- ^ The open (fst) and close (snd) tags
             -> [Int] -- ^ *Sorted* list of 0-based indices at which to insert tags.
             -> [Token Text]
-- maybe this should've been implemented with iterate instead?
insertTags s (open,close) idx =
    snd $ mapAccumL f idx (zip s [0..])
    where f []            (stoken,_)                    = ([], stoken)
          f idx'@(i:idxs) (stoken,sindex) | i == sindex = (idxs, merge [open,stoken,close])
                                          | otherwise   = (idx', stoken)
          -- CAREFUL: This will squish any kind of Token into a Word
          merge :: [Token Text] -> Token Text
          merge t = Word { surface = T.concat . fmap surface $ t
                         , lemma   = T.concat . fmap lemma   $ t
                         , pos     = T.concat . fmap pos     $ t }

tagSentence :: Sentence Text -> PFEG st (Sentence Text)
tagSentence s = do
    ts <- liftM targets ask
    let target_indices = findIndices ((`elem` ts).surface) s
    return $ insertTags s (o_targetTag, c_targetTag) target_indices

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

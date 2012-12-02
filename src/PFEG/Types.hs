{-# LANGUAGE FlexibleInstances, DeriveFunctor, DeriveFoldable, DeriveTraversable  #-}
module PFEG.Types
    ( Sentence
    , Token(..)
    , Document
    , nonWord
    ) where

import Data.Traversable (Traversable)
import Data.Foldable (Foldable)

-- | A Sentence is a list of tokens.
type Sentence a = [Token a]

-- | Produce a "word" @Token@ which has the same data in all three fields.
--   This can be used to represent a tag or a NULL token etc.
nonWord :: a -> Token a
nonWord x = Word x x x 

-- | Represents either a normal word token, or a masked one.
data Token a = Word   { pos :: !a, lemma :: !a, surface :: !a }
             | Masked { pos :: !a
                      , lemma :: !a
                      , surface :: !a
                      , original :: !a
                      , alternatives :: ![a] } deriving (Show,Functor,Traversable,Foldable,Eq)

-- | Represents a WAC document.
type Document a = [Sentence a]

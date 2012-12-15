{-# LANGUAGE FlexibleInstances, DeriveFunctor, DeriveFoldable, DeriveTraversable  #-}
module PFEG.Types
    ( Sentence
    , Token(..)
    , Document
    , nonWord
    , Match(..)
    , Level(..)
    , MatchPattern(..)
    , showShort
    , Context(..)
    , Item
    , PFEGConfig(..)
    , ModeConfig(..)
    , Regexes(..)
    , Corpus
    , Name
    ) where

import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
import Data.Hashable
import Data.Text (Text)

import Control.Concurrent.Chan
import Data.Text.ICU.Convert
import Data.Text.ICU (Regex)
import System.IO
import Database.HDBC.MySQL

import qualified Text.Search.Sphinx as S

-- | A Sentence is a list of tokens.
type Sentence a = [Token a]

-- | Produce a "word" @Token@ which has the same data in all three fields.
--   This can be used to represent a tag or a NULL token etc.
nonWord :: a -> Token a
nonWord x = Word x x x 

-- | Represents either a normal word token, or a masked one.
data Token a = Word   { pos :: !a, lemma :: !a, surface :: !a }
               deriving (Show,Functor,Traversable,Foldable,Eq)

-- | Represents a WAC document.
type Document a = [Sentence a]

data MatchPattern = MatchPattern { leftWindow  :: !Match
                                 , rightWindow :: !Match
                                 , level :: !Level
                                 , tolerance :: !Int
                                 , weight :: !Double }

-- | Two patterns are equal even if their weights differ
instance Eq MatchPattern where
    m == m' = leftWindow m == leftWindow m' && rightWindow m == rightWindow m' && level m == level m' && tolerance m == tolerance m'

-- | Weights don't factor into the hash, so we'll score them the same. Note
-- that hashes are only guaranteed to be unique for context windows < 20
-- and tolerances < 5.
instance Hashable MatchPattern where
    hash m = let x = 20 * size (leftWindow m) + size (rightWindow m) + 5 * tolerance m 
              in if level m == Surface then x else x*x

data Match = Match { size :: !Int } deriving Eq

data Level = Surface | Lemma deriving Eq

instance Show Level where
    show Surface = "S"
    show Lemma   = "L"

instance Show Match where
    show Match { size = s } = show s

instance Show MatchPattern where
    show mp = showShort mp ++ '|':show (weight mp)

-- | Like "MatchPattern"'s "Show" instance, but without showing the weight
showShort :: MatchPattern -> String
showShort mp = show (level mp) ++ show (leftWindow mp) ++ '-':show (rightWindow mp) ++ '~':show (tolerance mp)

data Context a = Context { left  :: ![a] , right :: ![a] } deriving (Functor,Show,Foldable,Traversable)

type Item a = (Token a,Context (Token a))

type Name = String
type Corpus = (Name,FilePath)

data PFEGConfig = PFEGConfig
    { pfegMode         :: ModeConfig -- ^ Program mode specific configuration
    , statusLine       :: Chan Int -- ^ Status update channel
    , database         :: Connection -- ^ The connection to the main database
    , corpusConverter  :: Converter -- ^ Text.ICU input encoding converter
    , targets          :: [Text] -- ^ Targets for this run
    , majorityBaseline :: Text
    , sphinxIndex      :: Text
    , sample           :: (Int,Int)
    , searchConf       :: S.Configuration
    , cardRegexes      :: Regexes
    , debugLog         :: Handle -- ^ Write debug information to this log
    , chunkSize        :: Int -- ^ Chunk size for the Iteratee
    , matchPatterns    :: [MatchPattern] }

data ModeConfig = Record  { corpora :: [Corpus] }
                | Predict { corpus  :: Corpus, predictLog :: Handle }
                | Learn   { corpus  :: Corpus, statLog :: Handle }

data Regexes = Regexes
     { numeralRegex :: Regex -- ^ when tagged CARD and matching this, tokens are left as is
     , dateRegex    :: Regex -- ^ when tagged CARD and matching this, surface is DATE
     , timeRegex    :: Regex -- ^ when tagged CARD and matching this, surface is TIME
     , cardTag :: Text -- ^ The pos tag that represents cardinalities
     }


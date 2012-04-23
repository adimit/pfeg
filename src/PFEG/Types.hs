{-# LANGUAGE FlexibleInstances, DeriveFunctor, DeriveFoldable, DeriveTraversable  #-}
module PFEG.Types
    ( Sentence
    , Token(..)
    , MatchMode(..)
    , MatchPattern(..)
    , parsePattern
    ) where

import Text.ParserCombinators.Parsec
import Data.Bits
import Data.Hashable
import Data.List (foldl')
import Control.Monad (liftM)

import Data.Traversable (Traversable)
import Data.Foldable (Foldable)

type Sentence a = [Token a]

data Token a = Word   { pos :: !a, lemma :: !a, surface :: !a }
             | Masked { pos :: !a
                      , lemma :: !a
                      , surface :: !a
                      , original :: !a
                      , alternatives :: ![a] } deriving (Show,Functor,Traversable,Foldable)

parsePattern :: String -> Either ParseError MatchPattern
parsePattern = parse patternParser "match pattern"

patternParser :: Parser MatchPattern
patternParser =
    let parseLetter c m = char c >> return (Just m)
        parseNothing    = char '_' >> return Nothing
    in liftM MatchPattern $ spaces >> many1 (parseLetter 'L' L
                                        <|> parseLetter 'P' P
                                        <|> parseLetter 'S' S
                                        <|> parseNothing)

data MatchMode = P | L | S deriving (Show,Eq)
newtype MatchPattern = MatchPattern { unMatchPattern :: [Maybe MatchMode] } deriving (Eq)

instance Show MatchPattern where
    show = Prelude.take 6.concatMap (maybe "_" show).(++ repeat Nothing).unMatchPattern

instance Hashable MatchPattern where
    hash = foldl' f 1.unMatchPattern
       where f x (Nothing) = shift x 2
             f x (Just P ) = shift x 2 .|. 1
             f x (Just L ) = shift x 2 .|. 2
             f x (Just S ) = shift x 2 .|. 3

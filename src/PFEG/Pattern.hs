{-# LANGUAGE TupleSections, OverloadedStrings #-}
module PFEG.Pattern
    ( -- * Pattern data types
      -- | Types for patterns inside @PFEG@
      MatchPattern(..)
    , Match(..)
    , Level(..)
      -- ** Parsing and rendering functions for patterns
    , showShort
    , parsePattern
    , makeQuery
    , patternRestriction
    ) where

import PFEG.Types
import Text.Parsec.Text
import Data.Text (Text)
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator
import PFEG.Context
import Control.Monad (liftM)
import qualified Data.Text as T

patternRestriction :: MatchPattern -> Restriction
patternRestriction MatchPattern { leftWindow = Match { size = l } , rightWindow = Match { size = r } } = (l,r)

parseWeight :: Parser Double
parseWeight = do
    numString <- char '|' >> many1 (digit <|> char '.')
    return $ read numString

parseLevel :: Parser Level
parseLevel = (char 'L' >> return Lemma) <|> (char 'S' >> return Surface)

parseMatch :: Parser Match
parseMatch = do
    sz  <- liftM read $ many1 digit
    return Match { size = sz }

parsePattern :: Parser MatchPattern
parsePattern = do
    lvl <- parseLevel
    l <- parseMatch
    r <- char '-' >> parseMatch
    tol <- option "1" (try (char '~' >> many1 digit))
    w <- option 1.0 parseWeight
    return MatchPattern { level = lvl, leftWindow = l, rightWindow = r, tolerance = read tol, weight = w }

makeQuery :: Context (Token Text) -> MatchPattern -> Text -> Text
makeQuery cxt' p t =
    let Context { left = leftContext, right = rightContext} = retrieveWords' cxt' p
    in T.unwords [renderLevel (level p), T.concat [wrap '"' (T.unwords (leftContext ++ t:rightContext)), renderTolerance (tolerance p)]]

retrieveWords' :: Context (Token a) -> MatchPattern -> Context a
retrieveWords' cxt p = fmap lvl (restrictContext (patternRestriction p) cxt)
    where lvl = case level p of 
                     Surface -> surface
                     Lemma -> lemma

renderLevel :: Level -> Text
renderLevel Surface = "@surface"
renderLevel Lemma   = "@lemma"

renderTolerance :: Int -> Text
renderTolerance t | t <= 1    = T.empty
                  | otherwise = if t > 0 then T.pack ('~':show t) else T.empty

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
import PFEG.Context (Restriction,Context)
import qualified PFEG.Context as C
import Control.Monad (liftM)
import qualified Data.Text as T
import Data.Hashable

data MatchPattern = MatchPattern { left  :: !Match
                                 , right :: !Match
                                 , level :: !Level
                                 , tolerance :: !Int
                                 , weight :: !Double }

-- | Two patterns are equal even if their weights differ
instance Eq MatchPattern where
    m == m' = left m == left m' && right m == right m' && level m == level m' && tolerance m == tolerance m'

-- | Weights don't factor into the hash, so we'll score them the same. Note
-- that hashes are only guaranteed to be unique for context windows < 20
-- and tolerances < 5.
instance Hashable MatchPattern where
    hash m = let x = 20 * size (left m) + size (right m) + 5 * tolerance m 
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
showShort mp = show (level mp) ++ show (left mp) ++ '-':show (right mp) ++ '~':show (tolerance mp)

patternRestriction :: MatchPattern -> Restriction
patternRestriction MatchPattern { left = Match { size = l } , right = Match { size = r } } = (l,r)

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
    return MatchPattern { level = lvl, left = l, right = r, tolerance = read tol, weight = w }

makeQuery :: C.Context (Token Text) -> MatchPattern -> Text -> Text
makeQuery cxt' p t =
    let C.Context { C.left = leftContext, C.right = rightContext} = retrieveWords' cxt' p
    in T.unwords [renderLevel (level p), T.concat [wrap '"' (T.unwords (leftContext ++ t:rightContext)), renderTolerance (tolerance p)]]

retrieveWords' :: Context (Token a) -> MatchPattern -> Context a
retrieveWords' cxt p = fmap lvl (C.restrictContext (patternRestriction p) cxt)
    where lvl = case level p of 
                     Surface -> surface
                     Lemma -> lemma

renderLevel :: Level -> Text
renderLevel Surface = "@surface"
renderLevel Lemma   = "@lemma"

wrap :: Char -> Text -> Text
wrap c x = T.cons c $ T.concat [x, T.singleton c]

renderTolerance :: Int -> Text
renderTolerance t | t <= 1    = T.empty
                  | otherwise = if t > 0 then T.pack ('~':show t) else T.empty

{-# LANGUAGE OverloadedStrings #-}
module PFEG.Pattern
    ( -- * Pattern data types
      -- | Types for patterns inside @PFEG@
      MatchPattern(..)
    , Match(..)
    , Level(..)
    , MatchData(..)
    , Interference(..)
      -- ** Parsing and rendering functions for patterns
    , parsePattern
    , renderAsSphinx
    , matchParser
    , makeQuery
    , patternRestriction
    ) where

import PFEG.Types
import PFEG.ShortestMatch
import Text.Parsec.Text
import Data.Text (Text)
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator
import PFEG.Context (Restriction,Item,Context)
import qualified PFEG.Context as C
import Control.Monad (liftM)
import qualified Data.Text as T

data MatchData = MatchData
    { predictedTarget  :: Token Text
    , interferingWords :: Interference (Token Text)
    } deriving Show

data MatchPattern = MatchPattern { left  :: Match
                                 , right :: Match
                                 , centerInterference  :: Int
                                 , level :: Level}

data Match = Match { size :: Int, tolerance :: Int }

data Level = Surface | Lemma

instance Show Level where
    show Surface = "S"
    show Lemma   = "L"

instance Show Match where
    show Match { size = s, tolerance = t } = show s ++ '~':show t

instance Show MatchPattern where
    show mp = show (level mp) ++ show (left mp) ++ '-':show (centerInterference mp) ++ '-':show (right mp)

patternRestriction :: MatchPattern -> Restriction
patternRestriction MatchPattern { left = Match { size = l } , right = Match { size = r } } = (l,r)

parseLevel :: Parser Level
parseLevel = (char 'L' >> return Lemma) <|> (char 'S' >> return Surface)

parseMatch :: Parser Match
parseMatch = do
    sz  <- liftM read $ many1 digit
    tol <- option 1 (try (char '~' >> liftM read (many1 digit)))
    return Match { size = sz, tolerance = tol }

parsePattern :: Parser MatchPattern
parsePattern = do
    lvl <- parseLevel
    l <- parseMatch
    inter <- char '-' >> many1 digit
    r     <- char '-' >> parseMatch
    return MatchPattern { level = lvl, left = l, right = r, centerInterference = read inter }

-- | Given an item i and pattern p, construct a query for the Sphinx server
-- that looks for something similar to i as defined by p.
renderAsSphinx :: C.Item Text -> MatchPattern -> Text
renderAsSphinx i p =
    let (lwords,rwords) = retrieveWords i p
    in T.concat [ pr lwords (left p)
                , T.pack ('-':show (centerInterference p))
                , pr rwords (right p) ]
    where pr ws m = T.concat [ wrap '"' . T.intercalate " " $ renderLevel (level p):ws
                             , renderTolerance (tolerance m) ]

makeQuery :: C.Context (Token Text) -> MatchPattern -> Text
makeQuery cxt' p =
    let cxt = retrieveWords' cxt' p
    in T.intercalate " " [ pr (C.left cxt) (left p)
                         , T.pack "<<<"
                         , pr (C.right cxt) (right p)]
    where pr ws m = T.concat [ wrap '"' . T.intercalate " " $ renderLevel (level p):ws
                             , renderTolerance (tolerance m) ]

retrieveWords' :: Context (Token a) -> MatchPattern -> Context a
retrieveWords' cxt p = fmap lvl (C.restrictContext (patternRestriction p) cxt)
    where lvl = case level p of 
                     Surface -> surface
                     Lemma -> lemma

matchParser :: [Text] -> Context (Token Text) -> Sentence Text -> [MatchData]
matchParser targets cxt s =
    let prediction = findTarget ((`elem` targets).surface) (C.left cxt, C.right cxt) s
    in  maybe [] pred2match prediction

pred2match :: Prediction (Token Text) -> [MatchData]
pred2match (Prediction possT f_inter) = map (\t -> MatchData t (f_inter t)) possT

renderLevel :: Level -> Text
renderLevel Surface = "@surface"
renderLevel Lemma   = "@lemma"

chooseLevel :: Level -> Item a -> Context a
chooseLevel Surface = C.itemSurface
chooseLevel Lemma   = C.itemLemma

wrap :: Char -> Text -> Text
wrap c x = T.cons c $ T.concat [x, T.singleton c]

renderTolerance :: Int -> Text
renderTolerance t | t <= 1    = T.empty
                  | otherwise = if t > 0 then T.pack ('~':show t) else T.empty

retrieveWords :: Item a -> MatchPattern -> ([a],[a])
retrieveWords i MatchPattern { left = l, right = r, level = lvl } =
    ( reverse . take (size l) . reverse . C.left  . chooseLevel lvl $ i
    ,           take (size r)           . C.right . chooseLevel lvl $ i )

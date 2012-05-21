module PFEG.Pattern where

import Text.Parsec.Text
import Data.Text.ICU (Regex)
import Data.Text (Text)
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator

data MatchPattern = MatchPattern { left  :: Match
                                 , right :: Match
                                 , centerInterference  :: Int }

data Match = Match { form :: [Level], tolerance :: Int }

data Level = Surface | Lemma

instance Show Level where
    show Surface = "s"
    show Lemma   = "l"

instance Show Match where
    show Match { form = levels, tolerance = t } = foldl (\a l -> a ++ show l) "" levels ++ show t

instance Show MatchPattern where
    show mp = show (left mp) ++ '-':show (centerInterference mp) ++ '-':show (right mp)

parseLevel :: Parser Level
parseLevel = (char 'l' >> return Lemma) <|> (char 's' >> return Surface)

parseMatch :: Parser Match
parseMatch = do
    lvls <- many1 parseLevel
    tol  <- many1 digit
    return Match { form = lvls, tolerance = read tol }

parsePattern :: Parser MatchPattern
parsePattern = do
    l <- parseMatch
    inter <- char '-' >> many1 digit
    r     <- char '-' >> parseMatch
    return MatchPattern { left = l, right = r, centerInterference = read inter }

renderAsSphinx :: MatchPattern -> Text
renderAsSphinx = undefined

renderAsRegex :: MatchPattern -> Regex
renderAsRegex = undefined

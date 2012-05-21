module PFEG.Pattern where

import Text.Parsec.Text
import Data.Text.ICU (Regex)
import Data.Text (Text)

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

parsePattern :: Parser MatchPattern
parsePattern = undefined

renderAsSphinx :: MatchPattern -> Text
renderAsSphinx = undefined

renderAsRegex :: MatchPattern -> Regex
renderAsRegex = undefined

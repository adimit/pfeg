module PFEG.Pattern where

import Text.Parsec.Text
import Data.Text.ICU (Regex)
import Data.Text (Text)

data MatchPattern = MatchPattern { left  :: Match
                                 , right :: Match
                                 , centerInterference  :: Int }

data Match = Match { form :: [Level], tolerance :: Int }

data Level = Surface | Lemma

instance Show MatchPattern where
    show mp = undefined

parsePattern :: Parser MatchPattern
parsePattern = undefined

renderAsSphinx :: MatchPattern -> Text
renderAsSphinx = undefined

renderAsRegex :: MatchPattern -> Regex
renderAsRegex = undefined

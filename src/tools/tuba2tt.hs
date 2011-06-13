module Main where

{- program to convert tuba corpus to tt format. -}

import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as SB
import Text.XML.Expat.SAX
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)

type Sax t = SAXEvent t t

nl, tab, t_sentence, t_word, a_form, a_pos :: SB.ByteString
t_sentence = SB.pack "sentence"
t_word     = SB.pack "word"
a_form     = SB.pack "form"
a_pos      = SB.pack "pos"
nl         = SB.singleton '\n'
tab        = SB.singleton '\t'

transform :: Sax SB.ByteString -> SB.ByteString
transform (EndElement t) | t == t_sentence = nl
transform e@(StartElement t as) | t == t_word =
    fromMaybe (error $ "Invalid word " ++ show e)
              (do form <- a_form `lookup` as
                  pos  <- a_pos  `lookup` as
                  return $ SB.concat [form,tab,pos,nl])
transform _ = SB.empty

main :: IO ()
main = do (input:output:_) <- getArgs
          fmap (LB.fromChunks.map transform.parse defaultParseOptions)
               (LB.readFile input) >>= LB.writeFile output

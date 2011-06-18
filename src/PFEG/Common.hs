module PFEG.Common
    ( -- * Types
      Wordcounts
      -- * Configuration
    , Configuration(..)
    , standardConfig
      -- * Measuring execution times
    , doTimed
    , doTimed_
      -- * Attoparsec parsers for the TT-style corpora
    , wordP
    , sentenceP
    ) where

import PFEG.Types
import Data.Time.Clock
import qualified Data.HashMap.Strict as T
import qualified Data.Text as X
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Data.Attoparsec
import Control.Applicative hiding (many)
import Data.Word (Word8)

standardConfig :: Configuration
standardConfig = Config { lemmaTable   = "lemmas"
                        , posTable     = "pos"
                        , unigramTable = "unigrams"
                        , targets      = [ "in" , "von" , "mit", "für", "im"
                                         , "auf", "nach", "an" , "aus", "am"] }

doTimed :: IO a -> IO (a,NominalDiffTime)
doTimed f = do
    start  <- getCurrentTime
    result <- f
    end    <- getCurrentTime
    return (result, end `diffUTCTime` start)

doTimed_ :: IO a -> IO NominalDiffTime
doTimed_ f = fmap snd (doTimed f)
{-# INLINE doTimed_ #-}

wordP :: Parser (Word Text)
wordP = do surface <- takeTill (==tab8)
           tag     <- skip (==tab8) *> takeTill (==tab8)
           lemma   <- skip (==tab8) *> takeTill (==nl8)
           skip (==nl8)
           return $ ( X.toCaseFold.decodeUtf8 $ surface
                    ,              decodeUtf8   tag
                    , X.toCaseFold.decodeUtf8 $ lemma)

sentenceP :: Parser (Sentence Text)
sentenceP = do words <- wordP `manyTill` (word8 nl8) <* skipWhile (==nl8)
               return words

c28 :: Char -> Word8
c28 = fromIntegral.fromEnum
{-# INLINE c28 #-}

nl8, tab8 :: Word8
nl8  = c28 $! '\n'
tab8 = c28 $! '\t'

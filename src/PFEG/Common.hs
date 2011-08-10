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
      -- * Utility functions for context items
    , ctxMap
    , ctxMapM
      -- * SQL Utility functions
    , establishConnection
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

import Database.HDBC.Sqlite3

standardConfig :: Configuration
standardConfig = Config { lemmaTable   = "lemmas"
                        , posTable     = "pos"
                        , contextTable = "context7"
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
           return ( X.toCaseFold.decodeUtf8 $ surface
                  ,              decodeUtf8   tag
                  , X.toCaseFold.decodeUtf8 $ lemma)

sentenceP :: Parser (Sentence Text)
sentenceP = return =<< wordP `manyTill` word8 nl8 <* skipWhile (==nl8)

c28 :: Char -> Word8
c28 = fromIntegral.fromEnum
{-# INLINE c28 #-}

nl8, tab8 :: Word8
nl8  = c28 $! '\n'
tab8 = c28 $! '\t'

-- | Apply @g@ to all elements of the given context.
ctxMap :: (a -> b) -> Context a -> Context b
ctxMap g (Context3 a b c d e f) = Context3 (g a) (g b) (g c) (g d) (g e) (g f)
ctxMap g (Context2   b c d e  ) = Context2       (g b) (g c) (g d) (g e)
ctxMap g (Context1     c d    ) = Context1             (g c) (g d)

ctxMapM :: Monad m => (a -> m b) -> Context a -> m (Context b)
ctxMapM k (Context3 a b c d e f) =
       do (a':b':c':d':e':f':[]) <- mapM k [a,b,c,d,e,f]
          return $ Context3 a' b' c' d' e' f'
ctxMapM k (Context2   b c d e  ) =
       do (   b':c':d':e':   []) <- mapM k [  b,c,d,e  ]
          return $ Context2    b' c' d' e'
ctxMapM k (Context1     c d    ) =
       do (      c':d':      []) <- mapM k [    c,d    ]
          return $ Context1       c' d'

establishConnection :: String -> FilePath -> IO TableAccess
establishConnection tn fp = do
    conn <- connectSqlite3 fp
    return $ Access { connection = conn , table = tn }

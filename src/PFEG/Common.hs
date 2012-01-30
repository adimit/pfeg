{-# LANGUAGE BangPatterns #-}
module PFEG.Common
    ( -- * Types
      Wordcounts
      -- * Measuring execution times and logging
    , doTimed
    , doTimed_
    , renderS
    , logger
      -- * Attoparsec parsers for the TT-style corpora
    , wordP
    , sentenceP
      -- * Triplet operations (useful for @Word@
    , fst3
    , snd3
    , trd3
      -- * Shared iteratees
    , countChunksI
    , corpusI
      -- * Constants
    , chunk_size
    ) where

import PFEG.Types
import Data.Time.Clock
import qualified Data.Text as X
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)
import Control.Concurrent.Chan
import Data.Attoparsec.Iteratee
import Data.Iteratee.Base

import System.Time.Utils (renderSecs)

import qualified Data.Iteratee as I

import Data.Attoparsec
import Control.Applicative hiding (many)
import Data.Word (Word8)

import Prelude hiding (log)

import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)

chunk_size :: Int
chunk_size = (1024 :: Int) ^ (2 :: Int)

doTimed :: IO a -> IO (a,NominalDiffTime)
doTimed f = do
    start  <- getCurrentTime
    result <- f
    end    <- getCurrentTime
    return (result, end `diffUTCTime` start)

doTimed_ :: IO () -> IO NominalDiffTime
doTimed_ f = fmap snd (doTimed f)

renderS :: NominalDiffTime -> String
renderS = renderSecs.round

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

fst3 :: (a,b,c) -> a
fst3    (a,_,_) =  a
{-# INLINE fst3 #-}

snd3 :: (a,b,c) -> b
snd3    (_,b,_) =  b
{-# INLINE snd3 #-}

trd3 :: (a,b,c) -> c
trd3    (_,_,c) =  c
{-# INLINE trd3 #-}

corpusI :: (Monad m) => I.Iteratee ByteString m (Sentence Text)
corpusI = parserToIteratee sentenceP

countChunksI :: Chan Int -> I.Iteratee ByteString IO ()
countChunksI log = I.liftI (step 0)
    where step (!i) (Chunk _) = let i' = i+1
                                in liftIO (writeChan log i') >> I.liftI (step i')
          step _    stream    = I.idone () stream

logger :: Int -> UTCTime -> Chan Int -> IO ()
logger etc startTime logVar = forever log -- who wants to be forever log?
    where log = do numChunks <- readChan logVar
                   currentT <- getCurrentTime
                   let numChunks' = fromIntegral numChunks
                       etc'       = fromIntegral etc
                       difference = currentT `diffUTCTime` startTime
                       eta        = difference / numChunks' * etc'
                       percent    = numChunks' * 100 / etc'
                   putStr $ "\rRunning for " ++ renderS difference
                             ++ "; did " ++ show numChunks
                             ++ " chunks; ("++ show (round percent :: Integer)
                             ++ "%) ETA: " ++ renderS (eta-difference) ++ "   "
                   hFlush stdout


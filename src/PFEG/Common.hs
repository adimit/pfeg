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
    , corpusI
      -- * Configuration
    , UnigramIDs
      -- * types
    , MatchMode(..)
    , MatchPattern(..)
    ) where

import PFEG.Types
import Data.Time.Clock
import qualified Data.Text as X
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Data.HashMap.Strict (HashMap)

import Data.ByteString (ByteString)
import Control.Monad (forever)
import Control.Concurrent.Chan
import Data.Attoparsec.Iteratee

import System.Time.Utils (renderSecs)

import qualified Data.Iteratee as I

import Data.Attoparsec
import Control.Applicative hiding (many)
import Data.Word (Word8)

import Prelude hiding (log)
import Data.Bits
import Data.Hashable

import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)

import Data.List (foldl')

data MatchMode = P | L | S deriving (Show,Eq)
newtype MatchPattern = MatchPattern { unMatchPattern :: [Maybe MatchMode] } deriving (Eq)

instance Show MatchPattern where
    show = Prelude.take 6.concatMap (maybe "_" show).(++ repeat Nothing).unMatchPattern

instance Hashable MatchPattern where
    hash = foldl' f 1.unMatchPattern
       where f x (Nothing) = shift x 2
             f x (Just P ) = shift x 2 .|. 1
             f x (Just L ) = shift x 2 .|. 2
             f x (Just S ) = shift x 2 .|. 3

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

logger :: Int -> Chan Int -> IO ()
logger etc logVar = do
    t0 <- getCurrentTime
    forever $ do
        numChunks <- readChan logVar
        tcur <- getCurrentTime
        let ρ   = fromIntegral numChunks / fromIntegral etc
            δ   = tcur `diffUTCTime` t0
            eta = (1-ρ) * δ
        putStr $ "\rRunning for " ++ renderS δ
                  ++ "; did " ++ show numChunks
                  ++ " chunks; ("++ show (round (100*ρ) :: Integer)
                  ++ "%) ETA: " ++ renderS eta ++ "   "
        hFlush stdout

type UnigramIDs = HashMap Text Int

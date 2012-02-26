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
      -- * types
    , MatchMode(..)
    , MatchPattern(..)
      -- * Misc
    , nullToken
    ) where

import PFEG.Types
import Data.Time.Clock
import qualified Data.Text as X
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Data.Maybe (catMaybes)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Monad.IO.Class (MonadIO,liftIO)
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

wordP :: Parser (Maybe (Word Text))
wordP = do surface <- takeTill (==tab8)
           tag     <- skip (==tab8) *> takeTill (==tab8)
           lemma   <- skip (==tab8) *> takeTill (==nl8)
           skip (==nl8)
           if B.head tag == c28 '$'
              then return Nothing
              else return $ Just ( normalize.decodeUtf8 $ surface
                                 ,              decodeUtf8   tag
                                 , normalize.decodeUtf8 $ lemma)

normalize :: Text -> Text
normalize = ensureNotEmpty . filterPoop . X.toCaseFold

nullToken :: Text
nullToken = X.pack "NIX"

ensureNotEmpty :: Text -> Text
ensureNotEmpty t | t == X.empty = nullToken
                 | otherwise   = t

filterPoop :: Text -> Text
filterPoop = X.filter (not.(`elem` "\"}{'-.)([],"))

sentenceP :: Parser (Sentence Text)
sentenceP = return.catMaybes =<< wordP `manyTill` word8 nl8 <* skipWhile (==nl8)

c28 :: Char -> Word8
c28 = fromIntegral.fromEnum
{-# INLINE c28 #-}

nl8, tab8 :: Word8
nl8  = c28 '\n'
tab8 = c28 '\t'

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

countChunksI :: (MonadIO m) => Chan Int -> I.Iteratee ByteString m ()
countChunksI log = I.liftI (step 0)
    where step (!i) (Chunk _) = let i' = i+1
                                in liftIO (writeChan log i') >> I.liftI (step i')
          step _    stream    = I.idone () stream

logger :: Int -> Chan Int -> IO ()
logger total logVar = do
    t0 <- getCurrentTime
    forever $ do
        cur <- readChan logVar
        tcur <- getCurrentTime
        let ρ   = fromIntegral cur / fromIntegral (total+1)
            δt  = tcur `diffUTCTime` t0
            eta = (recip ρ - 1) * δt
        putStr $ "\rRunning for " ++ renderS δt
                  ++ "; did " ++ show cur ++ "/" ++ show total
                  ++ " (" ++ show (round (100*ρ) :: Integer)
                  ++ "%) ETA: " ++ renderS eta ++ "   "
        hFlush stdout

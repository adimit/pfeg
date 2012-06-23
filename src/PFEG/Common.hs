{-# LANGUAGE BangPatterns #-}
module PFEG.Common
    ( -- * Measuring execution times and logging
      doTimed
    , doTimed_
    , renderS
    , logger
      -- * Attoparsec parsers for the TT-style corpora
    , tokenP
    , sentenceP
      -- * Triplet operations (useful for @Word@
    , fst3
    , snd3
    , trd3
      -- * Shared iteratees
    , countChunksI
    , corpusI
      -- * Misc
    , nullToken
    , modify'
    ) where

import PFEG.Types
import Data.Time.Clock
import qualified Data.Text as X
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Control.Monad.State.Strict
import Data.Maybe (catMaybes)

import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import Control.Concurrent.Chan
import Data.Attoparsec.Iteratee
import Data.Iteratee.Base

import System.Time.Utils (renderSecs)

import qualified Data.Iteratee as I

import qualified Data.Attoparsec as A
import Data.Attoparsec (Parser)
import Control.Applicative hiding (many)
import Data.Word (Word8)

import Prelude hiding (log)

import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)

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

decode :: Parser ByteString -> Parser Text
decode = liftM decodeUtf8

tokenP :: Parser (Maybe (Token Text))
tokenP = token <|> word
    where token = do ts <- A.string (B.pack "-*-MASKED-*-") *>
                        A.takeTill (\w -> char '-' w || char '\t' w) `A.sepBy` delimiter
                     let (forms,_:orig) = break (==originalMarker) (map decodeUtf8 ts)
                     finishToken (\p l -> Masked { pos = p
                                                , lemma = l
                                                , surface  = normalize (head forms)
                                                , original = normalize (head orig)
                                                , alternatives = map normalize (tail forms)})
          word = do s <- decode $ A.takeTill (char '\t')
                    finishToken (\p l -> Word { pos = p, lemma = l, surface = normalize s })
          finishToken f = do p <- decode $ A.skip (char '\t') *> A.takeTill (char '\t')
                             l <- decode $ A.skip (char '\t') *> A.takeTill (char '\n')
                             A.skip (char '\n')
                             if X.head p `elem` "$#`,:'()"
                                then return Nothing
                                else return . Just $ f p (normalize l)

normalize :: Text -> Text
normalize = ensureNotEmpty . filterPoop . X.toCaseFold

delimiter :: Parser ByteString
delimiter = A.string $ B.pack "-*-"

originalMarker :: Text
originalMarker = X.pack "ORG"

nullToken :: Text
nullToken = X.pack "NIX"

ensureNotEmpty :: Text -> Text
ensureNotEmpty t | t == X.empty = nullToken
                 | otherwise   = t

filterPoop :: Text -> Text
filterPoop = X.filter (not.(`elem` "\"}{)([],"))

sentenceP :: Parser (Sentence Text)
sentenceP = return.catMaybes =<< tokenP `A.manyTill` A.word8 (c28 '\n') <* A.skipWhile (char '\n')

char :: Char -> Word8 -> Bool
char c = (==c28 c)
{-# INLINE char #-}

c28 :: Char -> Word8
c28 = fromIntegral.fromEnum
{-# INLINE c28 #-}

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
        let ρ  = fromIntegral cur / fromIntegral (total+1)
            δt = tcur `diffUTCTime` t0
            η  = (recip ρ - 1) * δt -- η ⇔ eta ⇔ ETA, get it? GET IT?
        putStr $ "\rRunning for " ++ renderS δt
                  ++ "; did " ++ show cur ++ "/" ++ show total
                  ++ " (" ++ show (round (100*ρ) :: Integer)
                  ++ "%) ETA: " ++ renderS η ++ "   "
        hFlush stdout

-- | A strict update to monad state via @f@.
modify' :: MonadState a m => (a -> a) -> m ()
modify' f = do
    s <- get
    put $! f s


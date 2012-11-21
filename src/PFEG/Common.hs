{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, BangPatterns, DeriveDataTypeable #-}
module PFEG.Common
    ( -- * Measuring execution times and logging
      doTimed
    , doTimed_
    , renderS
    , logger
      -- * Attoparsec parsers for the WAC "XML" corpora
    , sentenceP
    , documentP
      -- * Triplet operations
    , fst3
    , snd3
    , trd3
      -- * Shared iteratees
    , countChunksI
    , documentI
    , ParseError(..)
      -- * Misc
    , restrictContextToPattern
    , modify'
      -- * Orphan Nullable instances for Text
    , Text
    ) where

import Data.Nullable
import Data.NullPoint
import Data.Typeable
import Control.Exception
import PFEG.Pattern
import PFEG.Context
import PFEG.Types
import Data.Time.Clock
import qualified Data.Text as X
import Data.Text (Text)

import Control.Monad.State.Strict
import Data.Maybe (catMaybes)

import Control.Concurrent.Chan
import Data.Iteratee.Base

import System.Time.Utils (renderSecs)

import qualified Data.Iteratee as I

import qualified Data.Attoparsec.Text as A
import Data.Attoparsec.Text (Parser)
import Control.Applicative hiding (many)

import Prelude hiding (log)

import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)

instance Nullable Text where
    nullC = X.null

instance NullPoint Text where
    empty = X.empty

restrictContextToPattern :: MatchPattern -> Context a -> Context a
restrictContextToPattern p = restrictContext (patternRestriction p)

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

normalize :: Text -> Text
normalize = filterPoop . X.toCaseFold

filterPoop :: Text -> Text
filterPoop x | x `elem` [".","?","!"] = x 
             | otherwise = X.filter (`elem` ['a'..'z'] ++ "äöüß-.") x

documentP :: Parser (Document Text)
documentP = liftM (filter (not.null)) $ textURLP *> sentenceP `A.manyTill` A.string "</text>\n"
textURLP :: Parser Text
textURLP = "<text id=\"" A..*> A.takeWhile (/= '"') A.<*. "\">\n"

sentenceP :: Parser (Sentence Text)
sentenceP = do
    _ <- A.string "<s>\n"
    ws <- wordP `A.manyTill` A.string "</s>\n"
    return . catMaybes $ ws

wordP :: Parser (Maybe (Token Text))
wordP = do
    s <- liftM normalize $ A.takeWhile (/= '\t') <* A.char '\t'
    p <- A.takeWhile (/= '\t') <* A.char '\t'
    l <- liftM normalize $ A.takeWhile (not . A.isEndOfLine) <* A.endOfLine
    let w = Word { pos = p, surface = s, lemma = l }
    return $! if wordIsOK w then Just w else Nothing

wordIsOK :: Token Text -> Bool
wordIsOK Word { surface = s } | X.null s = False
                              | X.any (=='.') $ X.init s = False
wordIsOK _ = True

fst3 :: (a,b,c) -> a
fst3    (a,_,_) =  a
{-# INLINE fst3 #-}

snd3 :: (a,b,c) -> b
snd3    (_,b,_) =  b
{-# INLINE snd3 #-}

trd3 :: (a,b,c) -> c
trd3    (_,_,c) =  c
{-# INLINE trd3 #-}

documentI :: (Monad m) => I.Iteratee Text m (Document Text)
documentI = parserToIteratee documentP

countChunksI :: (MonadIO m, Nullable a) => Chan Int -> I.Iteratee a m ()
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

-- The following is a verbatim copy of attoparsec-iteratee's code, but
-- included here to make the parser conversion work with Attoparsec.Text
-- parsers (the original works only with ByteString parsers.)
-- We can't use ByteString parsers directly because of encoding issues.
-- (Maybe we could anyway, but I don't care)

data ParseError
    = ParseError {errorContexts :: [String], errorMessage :: String}
    deriving (Show, Typeable)

instance Exception ParseError

-- | A function to convert attoparsec 'Parser's into 'Iteratee's.
parserToIteratee :: (Monad m) => Parser a -> Iteratee Text m a
parserToIteratee p =
    icont (f (A.parse p)) Nothing
  where
    f k (EOF Nothing) =
        case A.feed (k X.empty) X.empty of
          A.Fail _ err dsc -> I.throwErr (toException $ ParseError err dsc)
          A.Partial _ -> I.throwErr (toException EofException)
          A.Done rest v
              | X.null rest -> idone v (EOF Nothing)
              | otherwise -> idone v (Chunk rest)
    f _ (EOF (Just e)) = I.throwErr e
    f k (Chunk s)
        | X.null s = icont (f k) Nothing
        | otherwise =
            case k s of
              A.Fail _ err dsc -> I.throwErr (toException $ ParseError err dsc)
              A.Partial k' -> icont (f k') Nothing
              A.Done rest v -> idone v (Chunk rest)

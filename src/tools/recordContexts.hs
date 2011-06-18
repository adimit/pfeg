module Main where

import PFEG.Types
import PFEG.Common
import System.Environment (getArgs)

import Database.HDBC
import Database.HDBC.Sqlite3

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.ByteString (ByteString)

import Data.Iteratee.Char
import Data.Iteratee.IO
import Data.Iteratee.ListLike as LL
import qualified Data.Iteratee as I

import Data.Attoparsec
import Data.Attoparsec.Iteratee

import Data.Word (Word8)

import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Control.Applicative hiding (many)

establishConnections :: FilePath -> FilePath -> IO (TableAccess,TableAccess)
establishConnections unigramT contextT = do
    unigramC <- connectSqlite3 unigramT
    contextC <- connectSqlite3 contextT
    let unigramA = Access { connection = unigramC
                          , table = unigramTable standardConfig }
        contextA = Access { connection = contextC
                          , table = "contexts" }
    return (unigramA,contextA)

wordP :: Parser (Word Text)
wordP = do surface <- takeTill (==tab8)
           tag     <- skip (==tab8) *> takeTill (==tab8)
           lemma   <- skip (==tab8) *> takeTill (==nl8)
           skip (==nl8)
           return $ ( T.toCaseFold.decodeUtf8 $ surface
                    ,              decodeUtf8   tag
                    , T.toCaseFold.decodeUtf8 $ lemma)

sentenceP :: Parser (Sentence Text)
sentenceP = do words <- wordP `manyTill` (word8 nl8) <* skipWhile (==nl8)
               return words

corpusI :: (Monad m) => Iteratee ByteString m [Sentence Text]
corpusI = parserToIteratee (many sentenceP)

extractItems :: [Sentence Text] -> [Item Text Text]
extractItems (s:ss) = exS s ++ extractItems ss
    where exS s' = undefined

c28 :: Char -> Word8
c28 = fromIntegral.fromEnum

nl8, tab8 :: Word8
nl8  = c28 $ '\n'
tab8 = c28 $ '\t'

main :: IO ()
main = do
    (unigramT:contextT:corpus:_) <- getArgs
    (unigramA,contextA) <- establishConnections unigramT contextT
    return ()

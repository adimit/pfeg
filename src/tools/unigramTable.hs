{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)

import Data.List (sortBy,foldl')
import Data.Ord (comparing)

import qualified Data.Text as X
import Data.Text.Encoding (decodeUtf8)

import Data.Iteratee.Char
import Data.Iteratee.IO
import qualified Data.Iteratee as I
import qualified Data.Iteratee.Parallel as IP

import qualified Data.HashMap.Strict as T

import Database.HDBC
import Database.HDBC.Sqlite3

import System.Environment (getArgs)

import PFEG.Common

wordIter :: (Monad m, Functor m) => I.Iteratee [ByteString] m a -> I.Iteratee ByteString m a
wordIter f = I.joinI $ (enumLinesBS I.><> I.filter (not.B.null)) f

composition :: Monad m => I.Iteratee [ByteString] m Wordcounts
composition = I.foldl' folder T.empty

folder :: Wordcounts -> ByteString -> Wordcounts
folder t = foldl' (\t' b -> T.insertWith (+) b 1 t') t . convert
    where convert bs = let !(w:t:r:[]) = X.split (=='\t').decodeUtf8 $ bs
                       in (X.toCaseFold $! w):t:(X.toCaseFold r):[]

mapRedComp :: Monad m => Int -> I.Iteratee [ByteString] m Wordcounts
mapRedComp n = IP.mapReduce n (foldl' folder T.empty)

type Insertion = [(X.Text,Int)] -> IO ()
makeUpsert :: Connection -> IO Insertion
makeUpsert conn = do
    ins <- prepare conn $
      "INSERT OR REPLACE INTO "++ (unigramTable standardConfig) ++" (form,count) VALUES"
      ++ " (?,COALESCE((SELECT count FROM unigrams WHERE form == ?)+?,?))"
    return $ executeMany ins.map makeSQLFields

makeSQLFields :: (X.Text,Int) -> [SqlValue]
makeSQLFields (w,c) = [toSql w, toSql w, toSql c, toSql c]

main :: IO ()
main = do (table:file:_) <- getArgs
          putStrLn "Extracting unigrams… "
          (unimap,unitime) <- doTimed $ fileDriverVBuf 65536 (wordIter composition) file
          putStrLn $ "Exctracted " ++ show (T.size unimap) ++ " unigrams in "++show unitime
          sorttime <- doTimed_.print $ take 10.reverse.sortBy (comparing snd).T.toList $ unimap
          putStrLn $ ". Sorted in "++ show sorttime
          putStrLn $ "Opening database connection to '"++file++"'…"
          conn <- connectSqlite3 table
          insert <- makeUpsert conn
          putStrLn "Inserting… "
          inserttime <- doTimed_ $ insert (T.toList unimap) >> commit conn
          disconnect conn
          putStrLn $ "Committed in "++show inserttime

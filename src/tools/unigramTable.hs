module Main where

import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)

import qualified Data.Text as X
import Data.Text.Encoding (decodeUtf8)

import Data.Iteratee.Char
import Data.Iteratee.Base
import Data.Iteratee.IO
import qualified Data.Iteratee as I

import Database.HDBC
import Database.HDBC.Sqlite3

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Exception (bracket)

import Graphics.Vty.Terminal

import System.Environment (getArgs)

import PFEG.Common

import System.IO (hFileSize,withFile,IOMode(ReadMode))
import System.Time.Utils (renderSecs)

import Data.Time.Clock

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

{-
 - .schema
 - CREATE TABLE unigrams (id integer primary key autoincrement, f text, UNIQUE(f));
 -}

wordI :: Statement -> I.Iteratee ByteString IO ()
wordI insS = I.joinI $ (enumLinesBS I.><> I.filter (not.B.null)) (I.liftI step)
    where step (Chunk bsl) = let ts = concatMap convert bsl
                             in liftIO $ mapM_ (insert insS) ts
          step stream      = I.idone () stream

convert :: ByteString -> [X.Text]
convert bs = case X.split (=='\t').decodeUtf8 $ bs of
                  (w:t:r:[]) -> [ X.toCaseFold $! w, t, X.toCaseFold r ]
                  _           -> error $ "Wrong format bs: " ++ show bs

mkInsertStmt :: String -> String
mkInsertStmt tn = "INSERT OR IGNORE INTO " ++ tn ++ " (f) " ++ " VALUES (?) "

insert :: Statement -> X.Text -> IO ()
insert insS t = void $ execute insS [toSql t]

main :: IO ()
main = do
    (table:file:[]) <- getArgs
    logVar    <- newChan
    startTime <- getCurrentTime
    term      <- terminal_handle
    csize     <- withFile file ReadMode hFileSize
    let etc = (fromIntegral csize `div` chunk_size)+1 -- estimated chunk size
    void $ forkIO $ logger etc startTime logVar
    bracket (do putStr "Connecting…"
                unidb <- connectSqlite3 table
                hide_cursor term
                return unidb)
            (\unidb ->
             do show_cursor term
                putStrLn "Disconnecting…"
                disconnect unidb)
            (\unidb ->
             do insertStmt <- prepare unidb (mkInsertStmt "unigrams")
                insert insertStmt (X.pack "NULL")
                I.run =<< enumFile chunk_size file (I.sequence_
                    [ countChunksI logVar, wordI insertStmt ])
                putStrLn "\nDone.\nCommitting…"
                doTimed_ (commit unidb) >>= putStrLn.("Took "++).renderSecs.round
                putStrLn "Done.")


module Main where

import Prelude hiding (log)

import PFEG.Types
import PFEG.Common
import PFEG.SQL
import PFEG.Context

import System.Environment (getArgs)
import System.IO (hFileSize,withFile,IOMode(ReadMode))
import System.Time.Utils (renderSecs)

import Database.HDBC
import Database.HDBC.Sqlite3

import Data.Text (Text)

import Data.Iteratee.IO
import qualified Data.Iteratee as I

import Data.Time.Clock

import Control.Monad (void,when)
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Exception (bracket)

import Graphics.Vty.Terminal

import Codec.Digest.SHA (hash,Length (SHA256))

recordI' :: Int -> Statement -> Statement -> Statement -> I.Iteratee (Sentence Text) IO ()
recordI' i insH updS insS = I.mapChunksM_ $
    mapM_ record.getItems
    where record :: Item Text (Context Text) -> IO ()
          record item@(Item _ _ _ t) = do
              let h = hash SHA256 item
              rownum <- execute updS [toSql h, toSql i, toSql t]
              when (rownum == 0) (do
                  void $ execute insS [toSql h,toSql i, toSql t]
                  void $ execute insH $ toSql h:contexts2SQL item)
main :: IO ()
main = do
    (contextT:corpus:shard:_) <- getArgs
    logVar    <- newChan
    startTime <- getCurrentTime
    term      <- terminal_handle
    csize     <- withFile corpus ReadMode hFileSize

    let etc = (fromIntegral csize `div` chunk_size)+1 -- estimated chunk size
    putStrLn $ "Estimated amount of chunks: " ++ show etc

    putStrLn $ "Starting shard " ++ shard ++ " at " ++ show startTime
    void $ forkIO $ logger etc startTime logVar

    -- Two things are done in the opening and closing bracket: 
    --      connect/disconnect the databases
    --      Hide/show the cursor
    -- The cursor must be hidden, because otherwise the logging action is going to
    -- cause epileptic shock.
    bracket (do putStr "Connecting…"
                contextdb <- connectSqlite3 contextT
                hide_cursor term
                return contextdb)
            (\contextdb ->
             do show_cursor term
                putStrLn "Disconnecting…"
                disconnect contextdb)
            (\contextdb ->
             do insH <- prepare contextdb insertHash
                updS <- prepare contextdb updateWithHash
                insS <- prepare contextdb insertWithHash

                putStrLn "Connections established!\nRecording…"

                I.run =<< enumFile chunk_size corpus (I.sequence_
                    [ countChunksI logVar
                    , I.joinI $ I.convStream corpusI (recordI' (read shard) insH updS insS)])

                putStrLn "Done.\nCommitting…"
                doTimed_ (commit contextdb) >>= putStrLn.("Took "++).renderSecs.round
                putStrLn "Done.")

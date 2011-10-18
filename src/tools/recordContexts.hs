{-# LANGUAGE TupleSections,BangPatterns #-}
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

import Control.Monad (void,when,(>=>))
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Exception (bracket)

import Graphics.Vty.Terminal

recordI :: Statement -> DBStatements -> I.Iteratee (Sentence Text) IO ()
recordI lupS dbSQL = I.mapChunksM_ $
    (mapM_ $ indexItem lupS >=> recordItem dbSQL).getItems

recordItem :: DBStatements -> Item Text (Context Int) -> IO ()
recordItem (pSQL,lSQL,sSQL) (Item p l s t) = do
    pID <- recordContext pSQL (context2SQL p t)
    lID <- recordContext lSQL (toSql pID:context2SQL l t)
    recordContext_ sSQL (toSql lID:context2SQL s t)

recordContext :: Statements -> [SqlValue] -> IO ID
recordContext sql args = do
    rownum <- execute (updateStatement sql) args
    when (rownum == 0) (void $ execute (insertStatement sql) args)
    sqlvals <- execute (lookupStatement sql) args >> fetchRow (lookupStatement sql)
    case sqlvals of
        Just [rowid] -> return $ fromSql rowid
        Just x    -> error $ "Non-unique id; Database corrupt. "++ show x
        Nothing   -> error $ "Insertion failed. Database corrupt. "++show args

recordContext_ :: Statements -> [SqlValue] -> IO ()
recordContext_ sql args = do
    rownum <- execute (updateStatement sql) args
    when (rownum == 0) (void $ execute (insertStatement sql) args)

main :: IO ()
main = do
    (unigramT:contextT:corpus:_) <- getArgs
    logVar    <- newChan
    startTime <- getCurrentTime
    term      <- terminal_handle
    csize     <- withFile corpus ReadMode hFileSize

    let etc = ((fromIntegral csize) `div` chunk_size)+1 -- estimated chunk size
    putStrLn $ "Estimated amount of chunks: " ++ show etc

    putStrLn $ "Starting at " ++ show startTime
    void $ forkIO $ logger etc startTime logVar

    -- Two things are done in the opening and closing bracket: 
    --      connect/disconnect the databases
    --      Hide/show the cursor
    -- The cursor must be hidden, because otherwise the logging action is going to
    -- cause epileptic shock.
    bracket (do putStr "Connecting…"
                unigramA  <- establishConnection (unigramTable standardConfig) unigramT
                contextdb <- connectSqlite3 contextT
                hide_cursor term
                return (unigramA,contextdb))
            (\(unigramA,contextdb) ->
             do show_cursor term
                putStrLn "Disconnecting…"
                disconnect contextdb
                disconnect $ connection unigramA)
            (\(unigramA,contextdb) ->
             do lupS <- lookupIndexSQL unigramA
                cPStmts <- getcPStatements (Access contextdb "cP")
                cLStmts <- getc'Statements (Access contextdb "cL")
                cSStmts <- getc'Statements (Access contextdb "cS")

                putStrLn "Connections established!\nRecording…"

                I.run =<< enumFile chunk_size corpus (I.sequence_
                    [ countChunksI logVar
                    , I.joinI $ I.convStream corpusI (recordI lupS (cPStmts,cLStmts,cSStmts))])

                putStrLn "Done.\nCommitting…"
                doTimed_ (commit contextdb) >>= putStrLn.("Took "++).renderSecs.round
                putStrLn "Done.")

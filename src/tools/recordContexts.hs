{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude hiding (log)

import PFEG.Types
import PFEG.Common
import PFEG.SQL
import PFEG.Context

import System.Environment (getArgs)
import System.IO (hFileSize,withFile,IOMode(ReadMode))
import System.Time.Utils (renderSecs)

import qualified Data.HashMap.Strict as M

import Database.HDBC
import Database.HDBC.Sqlite3

import Data.Text (Text)

import Data.Text.Encoding (decodeUtf8)

import Data.Iteratee.IO
import qualified Data.Iteratee as I

import Data.Time.Clock

import Control.Monad (void,when,(>=>))
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Exception (bracket)

import Data.Convertible.Base (Convertible)

import Graphics.Vty.Terminal

recordI :: Statement -> UnigramTable -> DBStatements -> I.Iteratee (Sentence Text) IO ()
recordI lupS uniT dbSQL = I.mapChunksM_ $
    mapM_ (indexItem lupS uniT >=> recordItem dbSQL).getItems

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

readUnigrams :: Statement -> IO UnigramTable
readUnigrams stmt = execute stmt [] >> step 0 M.empty
    where step n t = if n < 2000000
                        then do row <- fetchRow stmt
                                case fmap f row of
                                     (Just (k,v)) -> step (n+1) (M.insert k v t)
                                     Nothing      -> return t
                        else return t
          f (s:c:[]) = (decodeUtf8.fromSql $ s, fromSql c)
          f _        = error "Stop doing silly things with the code."

selectAllSQL :: String
selectAllSQL = "SELECT form,id FROM unigrams"

main :: IO ()
main = do
    (unigramT:contextT:corpus:_) <- getArgs
    logVar    <- newChan
    term      <- terminal_handle
    startTime <- getCurrentTime
    csize     <- withFile corpus ReadMode hFileSize

    let etc = (fromIntegral csize `div` chunk_size)+1 -- estimated chunk size
    putStrLn $ "Estimated amount of chunks: " ++ show etc

    putStrLn $ "Starting at " ++ show startTime
    void $ forkIO $ logger etc startTime logVar

    -- Two things are done in the opening and closing bracket: 
    --      connect/disconnect the databases
    --      Hide/show the cursor
    -- The cursor must be hidden, because otherwise the logging action is going to
    -- cause epileptic shock.
    bracket (do putStrLn "Connecting…"
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

                selectAll <- prepare (connection unigramA) selectAllSQL
                putStrLn "Reading in unigrams…"
                (uniTable,time) <- doTimed $ readUnigrams selectAll
                putStrLn $ "Took " ++ renderS time
                finish selectAll

                putStrLn "Connections established!\nRecording…"

                I.run =<< enumFile chunk_size corpus (I.sequence_
                    [ countChunksI logVar
                    , I.joinI $ I.convStream corpusI (recordI lupS uniTable (cPStmts,cLStmts,cSStmts))])

                putStrLn "Done.\nCommitting…"
                doTimed_ (commit contextdb) >>= putStrLn.("Took "++).renderSecs.round
                putStrLn "Done.")

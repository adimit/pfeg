{-# LANGUAGE TupleSections,BangPatterns #-}
module Main where

import Prelude hiding (log)

import PFEG.Types
import PFEG.Common
import PFEG.SQL
import PFEG.Context
import qualified PFEG.BinaryMagic as Magic -- it's a kind of magic!

import System.Environment (getArgs)
import System.IO (hFileSize,withFile,IOMode(ReadMode))

import Database.HDBC
import Database.HDBC.Sqlite3

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import Data.Attoparsec.Iteratee
import Data.Iteratee.IO
import Data.Iteratee.Base
import qualified Data.Iteratee as I

import Data.Time.Clock
import Data.List (findIndices)
import Data.Maybe (fromMaybe)
import Data.Int (Int32)
import Data.Functor ((<$>))
import Safe (atMay)

import Control.Monad (forever,void,when,(>=>))
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Exception (bracket)

import Graphics.Vty.Terminal

corpusI :: (Monad m) => I.Iteratee ByteString m (Sentence Text)
corpusI = parserToIteratee sentenceP

type DBStatements = (Statements,Statements,Statements,Statements)

countChunksI :: Chan Int -> I.Iteratee ByteString IO ()
countChunksI log = I.liftI (step 0)
    where step (!noChunk) (Chunk _) = lift (writeChan log (noChunk+1) ) >> I.liftI (step $ noChunk+1)
          step _          stream    = I.idone ()  stream

recordI :: Statement -> DBStatements -> I.Iteratee (Sentence Text) IO ()
recordI lupS dbSQL = I.mapChunksM_ $
    (mapM_ $ indexItem lupS >=> (recordItem dbSQL . fmap Magic.encodePair)).getItems

recordItem :: DBStatements -> Item Text L.ByteString -> IO ()
recordItem (pSQL,lSQL,cSQL,sSQL) (Item p l c s t) = do
    pID <- recordContext pSQL (context2SQL p t)
    lID <- recordContext lSQL (toSql pID:context2SQL l t)
    case c of Just c' -> recordContext_ cSQL (toSql lID:context2SQL c' t)
              Nothing -> return () -- skip this.
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

lookupIndex :: Statement -> Text -> IO Int32
lookupIndex stmt t =
    do sqlvals <- execute stmt [toSql t] >> fetchRow stmt
       case sqlvals of
           Just [sqlval] -> return $ fromSql sqlval
           _ -> error $ '\'':T.unpack t ++ "' did not yield an index or too many."

indexItem :: Statement -> Item Text (Bracket Text) -> IO (Item Text (Bracket Int32))
indexItem stmt = return =<< mapMItem (indexContext stmt)

indexContext :: Statement -> Context (Bracket Text) -> IO (Context (Bracket Int32))
indexContext stmt c = return =<< mapMContext (mapMBracket $ lookupIndex stmt) c

getItems :: Sentence Text -> [Item Text (Bracket Text)]
getItems s = let target_indices = findIndices (\(w,_,_) -> w `elem` targets') s
             in  map (getItem s) target_indices

getItem :: Sentence Text -> Int -> Item Text (Bracket Text)
getItem s i = let nt          = T.pack "NULL" -- Special null-unigram
                  wordContext = Context3 (Bracket (a,f)) (Bracket (b,e)) (Bracket (c,d))
                  (a:b:c:t:d:e:f:[]) = map (fromMaybe (nt,nt,nt).atMay s) [i-3..i+3]
                  sItem'      = fmap fst3    <$> wordContext
                  cItem'      = fmap cardnnp <$> wordContext
              in  Item { pItem = fmap trd3   <$> wordContext
                       , lItem = fmap snd3   <$> wordContext
                       , sItem = sItem'
                       , cItem = if cItem' == sItem' then Just cItem' else Nothing
                       , target = fst3 t }
              where cardnnp (sfc,_,t) = if t `elem` [T.pack "CARD", T.pack "NNP"]
                                         then t else sfc

targets' :: [Text]
targets' = map T.pack $ targets standardConfig

chunk_size :: (Num a) => a
chunk_size = 65536

logger :: Int -> UTCTime -> Chan Int -> IO ()
logger etc startTime logVar = forever log -- who wants to be forever log?
    where log = do numChunks <- readChan logVar
                   currentT <- getCurrentTime
                   let numChunks' = fromIntegral numChunks
                       etc'       = fromIntegral etc
                       difference = currentT `diffUTCTime` startTime
                       eta        = difference / numChunks' * etc'
                       percent    = numChunks' * 100 / etc'
                   putStr $ "\rRunning for " ++ show (round difference)
                             ++ "; did " ++ show numChunks
                             ++ " chunks; ("++ show (round percent)
                             ++ "%) ETA: " ++ show (round $ eta-difference)
                   hFlush stdout

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
                cCStmts <- getc'Statements (Access contextdb "cC")
                cSStmts <- getc'Statements (Access contextdb "cS")

                putStrLn "Connections established!\nRecording…"

                I.run =<< enumFile chunk_size corpus (I.sequence_
                    [ countChunksI logVar
                    , I.joinI $ I.convStream corpusI (recordI lupS (cPStmts,cLStmts,cCStmts,cSStmts))])

                putStrLn "Done.\nCommitting…"
                doTimed_ (commit contextdb) >>= putStrLn.("Took "++).show
                putStrLn "Done.")

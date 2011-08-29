{-# LANGUAGE TupleSections,BangPatterns #-}
module Main where

import PFEG.Types
import PFEG.Common
import PFEG.SQL
import qualified PFEG.BinaryMagic as Magic
import PFEG.Context

import System.Environment (getArgs)

import Database.HDBC
import Database.HDBC.Sqlite3

import Data.ByteString (ByteString)

import Data.Iteratee.IO
import qualified Data.Iteratee as I

import qualified Data.ByteString.Lazy as L
import Data.Time.Clock

import Data.Attoparsec.Iteratee

import qualified Data.Text as T
import Data.Text (Text)

import Data.List (findIndices)

import Data.Maybe (fromMaybe)

import Data.Int (Int32)

import Control.Monad (forever,void,when,(>=>))
import Safe (atMay)

import Data.Functor ((<$>))

import Control.Monad.Trans.Class (lift)
import Control.Concurrent.MVar
import Control.Concurrent
import Data.Iteratee.Base

corpusI :: (Monad m) => I.Iteratee ByteString m (Sentence Text)
corpusI = parserToIteratee sentenceP

type DBStatements = (Statements,Statements,Statements,Statements)

recordAndLogI :: MVar Int -> Statement -> DBStatements -> I.Iteratee (Sentence Text) IO ()
recordAndLogI logVar lupS dbSQL =
    I.liftI $ step 0
    where indexAndRecordM :: Item Text (Bracket Text) -> IO ()
          indexAndRecordM = indexItem lupS >=> (recordItem dbSQL . fmap Magic.encodePair)
          step (!noSent) (Chunk s) =
               let c' = noSent+1
               in lift (logVar `putMVar` c' >> (mapM_ indexAndRecordM.getItems) s)
                    >> I.liftI (step c')
          step _ stream = I.idone () stream

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

estimated_sentences :: (Num a) => a
estimated_sentences = 5010000

logger :: UTCTime -> MVar Int -> IO ()
logger startTime logVar = forever log -- who wants to be forever log?
    where log = do numSent <- takeMVar logVar
                   currentT <- getCurrentTime
                   let numSent'   = fromIntegral numSent
                       difference = currentT `diffUTCTime` startTime
                       eta        = difference / numSent' * estimated_sentences
                       percent    = numSent' * 100 / estimated_sentences
                   putStr $ "\rRunning for " ++ show difference
                             ++ "; recorded " ++ show numSent
                             ++ "sentences; ("++ show percent
                             ++ "%) ETA: " ++ show eta

main :: IO ()
main = do
    (unigramT:contextT:corpus:_) <- getArgs
    logVar <- newEmptyMVar
    startTime <- getCurrentTime

    putStrLn $ "Starting at "++show startTime

    forkIO $ logger startTime logVar

    putStrLn "Connecting…"

    unigramA  <- establishConnection (unigramTable standardConfig) unigramT
    contextdb <- connectSqlite3 contextT
    lookupIndexStatement         <- lookupIndexSQL unigramA
    cPStmts <- getcPStatements (Access contextdb "cP")
    cTStmts <- getcPStatements (Access contextdb "cT")
    cSStmts <- getcPStatements (Access contextdb "cS")
    cCStmts <- getcPStatements (Access contextdb "cC")

    putStrLn "Connections established!\nRecording…"

    I.run =<< enumFile chunk_size corpus (I.joinI $ I.convStream corpusI
        (recordAndLogI logVar lookupIndexStatement (cPStmts,cSStmts,cTStmts,cCStmts)))

    putStrLn "Done.\nCommitting…"
    doTimed_ (commit contextdb) >>= putStrLn.("Took "++).show

    putStrLn "Done.\nDisconnecting…"
    disconnect contextdb
    disconnect $ connection unigramA

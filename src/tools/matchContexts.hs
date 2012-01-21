-- {-# LANGUAGE TupleSections,BangPatterns #-}
module Main where

import Data.Time.Clock
import Data.List (intercalate)
import Control.Monad.Reader
import Control.Monad.State
import GHC.IO.Handle (Handle)
import Data.Maybe (catMaybes)

import System.Environment (getArgs)
import System.IO (hPutStrLn,IOMode(ReadMode,AppendMode),hFileSize,withFile,openFile,hClose)
import System.Time.Utils (renderSecs)

import Data.Iteratee.IO
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Exception (bracket)

import Graphics.Vty.Terminal

import Data.Text (Text)
import qualified Data.Text as T

import Database.HDBC
import Database.HDBC.Sqlite3

import PFEG.Types hiding (Configuration,connection)
import PFEG.Context
import PFEG.Common hiding (Configuration)

data LogState = LogState { currentItem :: Int }

data Configuration = Configuration
    { testShard  :: Maybe Int
    , connection :: Connection
    , logVar     :: MVar LogData
    , sqlLogH    :: Handle
    , resultH    :: Handle }

data MatchMode = P | L | S deriving Show
type MatchPattern = [Maybe MatchMode]

data LogData = LogData 
    { logItem :: Item Text (Context Text)
    , logResults :: [(MatchPattern,Result)] }

instance Show LogData where
    show (LogData li lr) = undefined

type SQLString = String
type Result = [(Text,Int,Int)] -- list of possible predictions with associated counts.

matchI :: Iteratee (Sentence Text) (ReaderT Configuration IO) ()
matchI = I.mapChunksM_ $ mapM m . getItems
    where m :: Item Text (Context Text) -> ReaderT Configuration IO ()
          m i = do cf <- ask
                   results <- mapM (sqlQuery i) matchmodes >>= mapM match
                   liftIO $ putMVar (logVar cf) (LogData i (zip matchmodes results))

-- | Given an SQL query, return the @Result@ from the database — an ordered list of target-count
-- tuples.
match :: SQLString -> ReaderT Configuration IO Result
match s = do
    cf   <- ask
    (result,time) <- liftIO.doTimed $ liftM sqlToResult (quickQuery' (connection cf) s []) -- TODO: quickQuery or quickQuery' ?
    liftIO $ hPutStrLn (sqlLogH cf) ((renderSecs.round $ time) ++ " | " ++ s)
    return result
    where sqlToResult [] = []
          sqlToResult ((t:c:h:[]):xs) = (fromSql t, fromSql c, fromSql h):sqlToResult xs
          sqlToResult xs = error $ "Unexpected data format." ++ show xs

-- | Given a list of @MatchMode@s and an item, make the appropriate SQL string to retreive
-- item counts per target for this particular pattern.
sqlQuery :: Item Text (Context Text) -> [Maybe MatchMode] -> ReaderT Configuration IO SQLString
sqlQuery (Item (Context pI) (Context lI) (Context sI) _t) mm = do
    cf <- ask
    let excludeShard = case testShard cf of Just s  -> "t != " ++ show s ++ " AND "
                                            Nothing -> ""
    return $ "SELECT t,sum(c) AS sums,count(DISTINCT hash.h) FROM hash,ctxt WHERE hash.h==ctxt.h AND " ++
              excludeShard ++ intercalate " AND " pattern ++ " GROUP BY t ORDER BY sums DESC"
    where pattern           = catMaybes $ zipWith3 mmSelect mm (zip3 pI lI sI) ([1..]::[Int])
          f c s n           = Just $ c:show n ++ " == " ++ T.unpack s
          mmSelect (Just P) = f 'p'.fst3
          mmSelect (Just L) = f 'l'.snd3
          mmSelect (Just S) = f 's'.trd3
          mmSelect Nothing  = \_ _ -> Nothing

logResult :: Handle -> LogData -> StateT LogState IO ()
logResult _ _ = undefined

main :: IO ()
main = do
    (contextT:corpus:shard:_) <- getArgs
    statusV <- newChan
    resultV <- newEmptyMVar
    startTime <- getCurrentTime
    term <- terminal_handle
    csize <- withFile corpus ReadMode hFileSize

    let etc = (fromIntegral csize `div` chunk_size)+1
    putStrLn $ "Estimated amount of chunks " ++ show etc

    void $ forkIO $ logger etc startTime statusV

    bracket (do contextdb <- connectSqlite3 contextT
                hide_cursor term
                sqlLogH' <- openFile "sql.log" AppendMode
                resultH' <- openFile "result.log" AppendMode
                return (contextdb,sqlLogH',resultH'))
            (\(contextdb,sqlLogH',resultH') -> do
                show_cursor term
                disconnect contextdb
                hClose sqlLogH'
                hClose resultH')
            (\(contextdb,sqlLogH',resultH') -> do
                let cf = Configuration
                         { testShard = Just $ read shard
                         , connection = contextdb
                         , logVar = resultV
                         , sqlLogH = sqlLogH'
                         , resultH = resultH' }
                (runReaderT $ I.run =<< enumFile chunk_size corpus (I.joinI $ I.convStream corpusI matchI)) cf)

prettyMatchMode :: [Maybe MatchMode] -> String
prettyMatchMode = take 6.concatMap (maybe "_" show).(++ repeat Nothing)

-- preliminary list of matchmodes
matchmodes :: [[Maybe MatchMode]]
matchmodes = [ map Just [S,S,S,S,S,S]
             , map Just [L,L,L,L,L,L]
             , map Just [P,P,P,P,P,P]
             , Nothing : map Just [S,S,S,S]
             , Nothing : map Just [L,L,L,L]
             , Nothing : map Just [P,P,P,P]
             , Nothing : Nothing : map Just [S,S]
             , Nothing : Nothing : map Just [L,L]
             , Nothing : Nothing : map Just [P,P] ]

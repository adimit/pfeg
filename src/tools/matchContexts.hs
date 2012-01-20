-- {-# LANGUAGE TupleSections,BangPatterns #-}
module Main where

import Data.List (intercalate)
import Control.Monad.Reader
import Control.Monad.State
import GHC.IO.Handle (Handle)
import Data.Maybe (catMaybes)

import Data.Iteratee.IO
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I

import Control.Concurrent.MVar

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
    , targets    :: [Text]
    , connection :: Connection 
    , logVar     :: MVar LogData }

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
    liftIO $ liftM sqlToResult (quickQuery' (connection cf) s []) -- TODO: quickQuery or quickQuery' ?
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
    return $ "SELECT t,sum(c),count(DISTINCT hash.h) FROM hash,ctxt WHERE hash.h==ctxt.h AND " ++
              excludeShard ++ intercalate " AND " pattern ++ " GROUP BY t ORDER BY c DESC"
    where pattern           = catMaybes $ zipWith3 mmSelect mm (zip3 pI lI sI) ([1..]::[Int])
          f c s n           = Just $ c:show n ++ " == " ++ T.unpack s
          mmSelect (Just P) = f 'p'.fst3
          mmSelect (Just L) = f 'l'.snd3
          mmSelect (Just S) = f 's'.trd3
          mmSelect Nothing  = \_ _ -> Nothing

logResult :: Handle -> LogData -> StateT LogState IO ()
logResult _ _ = undefined

main :: IO ()
main = undefined -- (runReaderT $ I.run =<< enumFile chunk_size corpus (I.joinI $ I.convStream corpusI matchI)) cf

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

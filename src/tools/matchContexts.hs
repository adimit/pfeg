-- {-# LANGUAGE TupleSections,BangPatterns #-}
module Main where

import Data.List (intercalate)
import Control.Monad.Reader
import Control.Monad.State
import GHC.IO.Handle (Handle)
import Data.Maybe (catMaybes)

import Data.Text (Text)
import qualified Data.Text as T

import PFEG.Context
import PFEG.Common hiding (Configuration)

data LogState = LogState { currentItem :: Int }

data Configuration = Configuration
    { testShard :: [Int]
    , targets   :: [Text]
    , hashtable :: String
    , ctxttable :: String }

data MatchMode = P | L | S

type SQLString = String
type Result = [(Int,Text)] -- list of possible predictions with associated counts.

match :: Item Text (Context Text) -> [MatchMode] -> ReaderT Configuration IO Result
match = undefined

-- | Given a list of @MatchMode@s and an item, make the appropriate SQL string
-- to query the hash DB with.
sqlHash :: Item Text (Context Text) -> [Maybe MatchMode] -> Reader Configuration SQLString
sqlHash (Item (Context pI) (Context lI) (Context sI) _t) mm = do
    cf <- ask
    return $ "SELECT h FROM " ++ hashtable cf ++ " WHERE " ++ intercalate " AND " pattern
    where pattern           = catMaybes $ zipWith3 mmSelect mm (zip3 pI lI sI) ([1..]::[Int])
          f c s n           = Just $ c:show n ++ " == " ++ T.unpack s
          mmSelect (Just P) = f 'p'.fst3
          mmSelect (Just L) = f 'l'.snd3
          mmSelect (Just S) = f 's'.trd3
          mmSelect Nothing  = \_ _ -> Nothing


logResult :: Handle -> Item Text (Context Text) -> Result -> StateT LogState IO ()
logResult _ _ _ = undefined

{- SQL statement
 -
 -}

main :: IO ()
main = undefined
{-
import Prelude hiding (log)

import PFEG.Types
import PFEG.Common
import PFEG.SQL
import PFEG.Context

import System.Environment (getArgs)
import System.IO (hClose,openFile,hFileSize,withFile,IOMode(ReadMode,WriteMode))
import System.Time.Utils

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

import Control.Monad (liftM,forever,void,when,(>=>))
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Exception (bracket)

import Graphics.Vty.Terminal

import GHC.IO.Handle.FD (stdout)
-}

{-
data MatchMode = POS | LEM | CRD | SFC | NIL
type Match = Context MatchMode
type Count = Int
data Result = Correct Match Count -- ^ Found a match, and the prediction was correct.
            | Baseline Bool -- ^ Didn't find a match. Choose baseline and report if correct.
            | Wrong Match Count -- ^ Found a match, but the prediction was incorrect.

matchContext :: Statement -> -- ^ SQL lookup statement for unigram indices
               DBStatements -> -- ^ SQL statements for looking up cP cL cS and cC values
               Item Text (Context Text) -> -- ^ An Item to match
               IO Result
matchContext lup stmts i = do
    -- iBS <- liftM ((fmap.fmap) Magic.encodePair) (indexItem lup i)
    
    -- check for s1 s2 s3
    -- s2id <- check for s1 s2
    -- when s2id check for s1 s2 l3
    -- check for s1

    return undefined

matchI :: Handle -> Statement -> I.Iteratee (Sentence Text) IO ()
matchI outH lupS = I.mapChunksM_ $
    mapM_ (indexItem lupS >=> findMatches >=> logResult outH).getItems

findMatches :: Item Text (Context Int) -> IO Result
findMatches = undefined

logResult :: Handle -> Result -> IO ()
logResult h = undefined

main :: IO ()
main = do
    (unigramT:contextT:corpus:[]) <- getArgs
    logVar   <- newChan

    startTime <- getCurrentTime
    term      <- terminal_handle
    csize     <- withFile corpus ReadMode hFileSize

    let outS = corpus ++ ".log"
    outH      <- openFile outS WriteMode
    putStrLn $ "Logging results to "++outS

    let etc = (fromIntegral csize `div` chunk_size)+1 -- estimated chunk size
    putStrLn $ "Estimated amount of chunks: " ++ show etc

    putStrLn $ "Starting at " ++ show startTime
    void $ forkIO $ logger etc startTime logVar

    bracket (do putStr "Connecting…"
                unigramA  <- establishConnection (unigramTable standardConfig) unigramT
                contextdb <- connectSqlite3 contextT
                hide_cursor term
                return (unigramA,contextdb))
            (\(unigramA,contextdb) ->
             do show_cursor term
                putStrLn "Disconnecting…"
                disconnect contextdb
                hClose outH
                disconnect $ connection unigramA)
            (\(unigramA,contextdb) ->
             do lupS <- lookupIndexSQL unigramA
                I.run =<< enumFile chunk_size corpus (I.sequence_
                    [ countChunksI logVar
                    , I.joinI $ I.convStream corpusI (matchI outH lupS) ])
                putStrLn "Done.")

-}

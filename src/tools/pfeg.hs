{-# LANGUAGE ScopedTypeVariables,BangPatterns #-}
module Main where

import PFEG.SQL
import PFEG.Common
import PFEG.Context

import Prelude hiding (log)

import System.Time.Utils (renderSecs)
import Data.List (intercalate)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import qualified Data.Iteratee as I
import Data.Iteratee.IO
import Data.Iteratee.Base
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)

import System.IO

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)

import Data.Time.Clock

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent (killThread,forkIO)
import Control.Exception (bracket)

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever,liftM,foldM,when,void)

import Database.HDBC
import Database.HDBC.Sqlite3

import Graphics.Vty.Terminal

import PFEG.Configuration
import ReadArgs

type PFEG a = ReaderT PFEGConfig IO a

data LogData = LogData
    { logItem    :: Item Text
    , logResults :: [(MatchPattern,Result)] }

main :: IO ()
main = do
    (mode :: String, configFile :: FilePath) <- readArgs
    bracket (do putStrLn "Initializing…"
                terminal_handle >>= hide_cursor
                configAttempt <- configurePFEG (mode == "match") configFile
                case configAttempt of
                     (Left err) -> error $ "Initialization failed: "++ show err
                     (Right config) -> return config)
            (\session -> do
                putStrLn "Shutting down…"
                terminal_handle >>= show_cursor
                deinitialize session)
            (\session -> do
                putStrLn "Running…"
                processor <- prepareProcessor session
                mapM_ (handleCorpus processor session) (corpora $ pfegMode session))

data SQL = RecordSQL { updateTarget  :: Statement
                     , insertContext :: Statement
                     , insertTarget  :: Statement }

indexItem :: UnigramIDs -> Item Text -> Item Int
indexItem udb i = (fromMaybe 1 . (`M.lookup` udb)) `fmap` i

countChunksI' :: Chan Int -> Iteratee ByteString (ReaderT PFEGConfig IO) ()
countChunksI' log = I.liftI (step 0)
    where step (!i) (Chunk _) = let i' = i+1
                                in liftIO (writeChan log i') >> I.liftI (step i')
          step _    stream    = I.idone () stream

data LogState = LogState { currentItem :: Int }

logResult :: Handle -> MVar LogData -> StateT LogState IO ()
logResult h resV = forever log -- who wants to be forever log
    where log = do (LogState n) <- get
                   liftIO $ takeMVar resV >>= hPutStr h.((show n ++ ": ")++).show >> hFlush h
                   put (LogState $ n+1)

type ItemProcessor = Item Text -> PFEG ()

handleCorpus :: ItemProcessor -> PFEGConfig -> Corpus -> IO ()
handleCorpus proc session (cName,cFile) = do
     logVar <- newChan
     t0     <- getCurrentTime
     csize  <- withFile cFile ReadMode hFileSize
     putStrLn $ "Processing '" ++ cName ++ "' at '" ++ cFile ++ ".'"
     threadID <- forkIO $ logger ((fromIntegral csize `div` chunk_size)+1) t0 logVar
     let iteratee = I.run =<< enumFile chunk_size cFile (I.sequence_
                        [ countChunksI' logVar
                        , I.joinI $ I.convStream corpusI (I.mapChunksM_ $ mapM proc.getItems (targets session))])
     runReaderT iteratee session
     killThread threadID
     case pfegMode session of
          (Record _) -> do
              putStr "\nCommitting…" >> hFlush stdout
              time <- doTimed_ (commit $ contextDB session) 
              putStrLn $ "\rCommitted in "++ (renderSecs.round $ time)
          (Match _ _ _) -> return ()

prepareProcessor :: PFEGConfig -> IO ItemProcessor
prepareProcessor session =
    case pfegMode session of
         (Record _) -> do
             insertCtxtS <- prepare (contextDB session) insertCtxtSQL
             insertTrgtS <- prepare (contextDB session) insertTargetSQL
             updateS     <- prepare (contextDB session) updateSQL
             let sql = RecordSQL { updateTarget  = updateS
                                 , insertContext = insertCtxtS
                                 , insertTarget  = insertTrgtS }
             return $ recordF sql
         m@(Match _ _ _) -> do
             sql <- precompileSQL mkMatchSQL (contextDB session) matchmodes
             logVar <- newEmptyMVar
             void . forkIO . void $ runStateT (logResult (resultLog m) logVar) (LogState 1)
             return $ matchF logVar (targetIDs m) sql

recordF :: SQL -> Item Text -> PFEG ()
recordF sql i = do
    cf <- ask
    let item'    = indexItem (unigramID cf) i
        pattern  = item2SQL item' -- just the slp forms
        pattern' = toSql (target item'):pattern -- the slp forms with target prepended
    numRows <- liftIO $ execute (updateTarget sql) pattern'
    when (numRows == 0) (do
         void.liftIO $ execute (insertContext sql) pattern
         void.liftIO $ execute (insertTarget  sql) pattern')

matchF :: MVar LogData -> IntMap Text -> MatcherInit -> Item Text -> PFEG ()
matchF logVar tids sql i = do
    results <- mapM (matchAPattern tids sql i) matchmodes
    liftIO $ putMVar logVar (LogData i (zip matchmodes results))

-- given a pattern, matcherInit and an Item, give a result from the database
-- helper function for @matchF@.
matchAPattern :: IntMap Text -> MatcherInit -> Item Text -> MatchPattern -> PFEG Result
matchAPattern tids sql i mm = do
    cf <- ask
    let pattern = item2SQLp mm (indexItem (unigramID cf) i)
    case mm `M.lookup` sql of
         Nothing -> error "IMPOSSIBRU!"
         (Just s) -> do
             void $ liftIO $ execute s pattern
             rows <- liftIO $ fetchAllRows' s
             return $ foldl f [] rows
             where f r (t:c:idc:[]) = (tids IM.! fromSql t,fromSql c,fromSql idc):r
                   f _ xs           = error $ "Unexpected data format." ++ show xs

-- FIXME: this needs a pretty printer
instance Show LogData where
    show (LogData (Item _ _ (Context s) t) lr) = intercalate " | " (map T.unpack s) ++ "\nActual: " ++ T.unpack t
        ++ "\n" ++ concatMap (\(mm,r) -> show mm ++ ": " ++ show r ++ "\n") lr

-- | list of possible predictions with associated counts and matches.
-- (target,count,amount_of_matches)
type Result = [(Text,Int,Int)]

-- | Map from @MatchPattern@s to SQL @Statement@s for the matcher.
type MatcherInit = HashMap MatchPattern Statement

precompileSQL :: (MatchPattern -> String) -> Connection -> [MatchPattern] -> IO MatcherInit
precompileSQL sqlMM conn = foldM f M.empty
    where f m mm = liftM (\s -> M.insert mm s m) (prepare conn (sqlMM mm))

-- preliminary list of matchmodes
matchmodes :: [MatchPattern]
matchmodes = map MatchPattern
             [ map Just [S,S,S,S,S,S]
             , map Just [L,L,L,L,L,L]
             , map Just [P,P,P,P,P,P]
             , Nothing : map Just [S,S,S,S]
             , Nothing : map Just [L,L,L,L]
             , Nothing : map Just [P,P,P,P]
             , Nothing : Nothing : map Just [S,S]
             , Nothing : Nothing : map Just [L,L]
             , Nothing : Nothing : map Just [P,P] ]

{-# LANGUAGE ScopedTypeVariables,BangPatterns,DeriveDataTypeable #-}
module Main where

import PFEG.SQL
import PFEG.Common
import PFEG.Context

import Prelude hiding (log)

import System.Time.Utils (renderSecs)
import Data.List.Split (splitOn)
import Data.List (intercalate)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import qualified Data.Iteratee as I
import Data.Iteratee.IO
import Data.Iteratee.Base
import Data.ByteString (ByteString)
import Data.Maybe (mapMaybe,fromMaybe)

import System.IO

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)

import Data.Time.Clock

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
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
    (mode' :: String, configFile :: FilePath) <- readArgs
    let mode = mode' == "match"
    putStrLn "Initializing…"
    handle mode configFile

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

handle :: Bool -> FilePath -> IO ()
handle mode configFile =
    bracket (do putStrLn "Initializing…"
                terminal_handle >>= hide_cursor
                configAttempt <- configurePFEG mode configFile
                case configAttempt of
                     (Left err) -> error $ "Initialization failed: "++ show err
                     (Right config) -> return config)
            (\session -> do
                putStrLn "Shutting down…"
                terminal_handle >>= show_cursor
                deinitialize session)
            (\session -> undefined)
             {- logVar <- newChan
                t0     <- getCurrentTime
                csize  <- withFile (corpus m) ReadMode hFileSize
                void $ forkIO $ logger ((fromIntegral csize `div` chunk_size)+1) t0 logVar
                void $ forkIO (void $ runStateT
                    (logResult (cLogFile session) (cLogVar session)) (LogState 1))
                processor <- prepareProcessor m session
                let targets' = map (T.strip . T.pack) $ splitOn "," (targets m)
                    iteratee = I.run =<< enumFile chunk_size (corpus m) (I.sequence_
                        [ countChunksI' logVar
                        , I.joinI $ I.convStream corpusI (I.mapChunksM_ $ mapM processor.getItems targets')])
                runReaderT iteratee session
                putStr "\nCommitting…"
                doTimed_ (commit $ cDatabase session) >>= putStrLn.("\rCommitted in "++).renderSecs.round) -}

type ItemProcessor = Item Text -> PFEG ()

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
         (Match _ _ _) -> undefined

{-
-- Prepare SQL, assemble the @ItemProcessor@ depending on which mode we're in.
prepareProcessor :: PFEGMain -> PFEGConfig -> IO ItemProcessor
prepareProcessor (Record _ _  _ _ _ _) session = do
    insertCtxtS <- prepare (cDatabase session) insertCtxtSQL
    insertTrgtS <- prepare (cDatabase session) insertTargetSQL
    updateS     <- prepare (cDatabase session) updateSQL
    let sql = RecordSQL { updateTarget  = updateS
                        , insertContext = insertCtxtS
                        , insertTarget  = insertTrgtS }
    return $ recordF sql
prepareProcessor (Match _ _ _ _ _ _) session = do
    sql <- precompileSQL mkMatchSQL (cDatabase session) matchmodes
    return $ matchF sql
-}

matchF :: MatcherInit -> Item Text -> PFEG ()
matchF sql i = do
    cf <- ask
    results <- mapM (matchAPattern sql i) matchmodes
    liftIO undefined -- putMVar (cLogVar cf) (LogData i (zip matchmodes results))

-- given a pattern, matcherInit and an Item, give a result from the database
-- helper function for @matchF@.
matchAPattern :: MatcherInit -> Item Text -> MatchPattern -> PFEG Result
matchAPattern sql i mm = do
    cf <- ask
    let matchMode = case pfegMode cf of (Record _) -> error "matchAPattern only works in match mode."
                                        x          -> x
    let pattern = item2SQLp mm (indexItem (unigramID cf) i)
    case mm `M.lookup` sql of
         Nothing -> error "IMPOSSIBRU!"
         (Just s) -> do
             void $ liftIO $ execute s pattern
             rows <- liftIO $ fetchAllRows' s
             return $ foldl f [] rows
             where f r (t:c:idc:[]) = (targetIDs matchMode IM.! fromSql t,fromSql c,fromSql idc):r
                   f _ xs           = error $ "Unexpected data format." ++ show xs

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

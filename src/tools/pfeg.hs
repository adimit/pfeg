{-# LANGUAGE BangPatterns,DeriveDataTypeable #-}
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

import System.Console.CmdArgs

import Graphics.Vty.Terminal

type PFEG a = ReaderT PFEGConfig IO a
data PFEGConfig = PFEGConfig
    { cUnigramIds :: UnigramIDs
    , cTargetIds  :: IntMap Text
    , cDatabase :: Connection
    , cLogVar  :: MVar LogData
    , cLogFile :: Handle }

data LogData = LogData
    { logItem    :: Item Text
    , logResults :: [(MatchPattern,Result)] }

{- START CMDARGS GIBBERISH -}

data PFEGMain = Record { corpus    :: FilePath
                       , unigrams  :: FilePath
                       , database  :: FilePath
                       , sqlLog    :: Maybe FilePath
                       , targets   :: String
                       , resultLog :: FilePath }
              | Match  { corpus    :: FilePath
                       , unigrams  :: FilePath
                       , database  :: FilePath
                       , sqlLog    :: Maybe FilePath
                       , targets   :: String
                       , resultLog :: FilePath }
              deriving (Data, Typeable)

instance Show PFEGMain where
    show (Record c u db sql ts _r) =
        standardMessage "Recording to" c u db sql ts ""
    show (Match  c u db sql ts r) =
        standardMessage "Matching aigainst" c u db sql ts ("Logging result to '" ++ r ++ "'")

standardMessage :: String -> FilePath -> FilePath -> FilePath ->
                  Maybe FilePath -> String -> String -> String
standardMessage m c u db sql ts r = m ++ " '" ++ db ++ "'\n" ++
                                      "from corpus '" ++ c ++ "'.\n" ++
                                      "unigrams are '" ++ u ++ "'\n" ++ r ++ "\n" ++
                                      "targets are '" ++ show ts ++ "'\n" ++
                                      case sql of
                                           Nothing  -> ""
                                           (Just x) -> "Logging SQL to '" ++ x ++ "'.\n"

recordCmd, matchCmd :: PFEGMain
recordCmd = Record { corpus    = commonCorpus def
                   , unigrams  = commonUnigrams "../db/de/uni.db"
                   , database  = commonDatabase "../db/de/ctx.db"
                   , sqlLog    = commonSqlLog def
                   , targets   = commonTargets
                   , resultLog = def &= typ "FILE" &= help "Log results to"}
                              &= help "Learn contexts from corpus and store in DB."

matchCmd  = Match  { corpus    = commonCorpus def
                   , unigrams  = commonUnigrams "../db/de/uni.db"
                   , database  = commonDatabase "../db/de/ctx.db"
                   , sqlLog    = commonSqlLog def
                   , targets   = commonTargets
                   , resultLog = "result.log" &= typ "FILE" &= help "Result log file location."}
                              &= help "Match context from corpus against DB."

commonTargets = "in,von,mit,für,im,auf,nach,an,aus,am"
    &= typ "TARGETLIST" &= help "Comma-separated list of targets, e.g 'in,von,mit…'"

commonCorpus, commonUnigrams, commonDatabase, commonSqlLog :: Data v => v -> v
commonCorpus   x = x &= typ "FILE" &= help "Input corpus in TT format."
commonUnigrams x = x &= typ "FILE" &= help "Location of the unigram index database."
commonDatabase x = x &= typ "FILE" &= help "Location of the Context database."
commonSqlLog   x = x &= typ "FILE" &= help "Output of SQL log, if desired."

mode :: Mode (CmdArgs PFEGMain)
mode = cmdArgsMode $ modes [matchCmd,recordCmd]
    &= help "Predicting Functional Elements in German (and others)"
    &= program "pfeg"
    &= summary "PFEG 0.1, Aleksandar Dimitrov 2012"

main :: IO ()
main = (\m -> print m >> handle m) =<< cmdArgsRun mode

{- END CMDARGS GIBBERISH -}

cacheHash :: Statement -> StateT UnigramIDs IO ()
cacheHash s = liftIO (void $ execute s []) >> fetchAll
    where fetchAll = do
          row <- liftIO $ fetchRow s
          case row of
               Nothing       -> return ()
               Just (f:i:[]) -> do
                   m <- get
                   put $! M.insert (fromSql f) (fromSql i) m
                   fetchAll
               _             -> fail "Malformed result in unigrams."

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

-- Mode-independent common initialization and cleanup code
handle :: PFEGMain -> IO ()
handle m =
    bracket (do putStrLn "Initializing."
                sv   <- newEmptyMVar
                cdb  <- connectSqlite3 (database m)
                cu   <- connectSqlite3 (unigrams m)
                s    <- prepare cu "SELECT f,id FROM unigrams"
                uids <- execStateT (cacheHash s) M.empty
                disconnect cu
                putStrLn "Done."
                terminal_handle >>= hide_cursor
                lf <- openFile (resultLog m) AppendMode
                let targets' = map (T.strip . T.pack) $ splitOn "," (targets m)
                return PFEGConfig { cUnigramIds = uids
                                    , cDatabase   = cdb
                                    , cTargetIds  = mkTargetIds targets' uids
                                    , cLogVar     = sv
                                    , cLogFile    = lf })
            (\session -> do
                terminal_handle >>= show_cursor
                putStrLn "Disconnecting…"
                hClose (cLogFile session)
                disconnect (cDatabase session))
            (\session -> do
                logVar <- newChan
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
                doTimed_ (commit $ cDatabase session) >>= putStrLn.("\rCommitted in "++).renderSecs.round)

mkTargetIds :: [Text] -> UnigramIDs -> IntMap Text
mkTargetIds ts uids = IM.fromList $ zip (mapMaybe (`M.lookup` uids) ts) ts

type ItemProcessor = Item Text -> PFEG ()

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

matchF :: MatcherInit -> Item Text -> PFEG ()
matchF sql i = do
    cf <- ask
    results <- mapM (matchAPattern sql i) matchmodes
    liftIO $ putMVar (cLogVar cf) (LogData i (zip matchmodes results))

-- given a pattern, matcherInit and an Item, give a result from the database
-- helper function for @matchF@.
matchAPattern :: MatcherInit -> Item Text -> MatchPattern -> PFEG Result
matchAPattern sql i mm = do
    cf <- ask
    let pattern = item2SQLp mm (indexItem (cUnigramIds cf) i)
    case mm `M.lookup` sql of
         Nothing -> error "IMPOSSIBRU!"
         (Just s) -> do
             void $ liftIO $ execute s pattern
             rows <- liftIO $ fetchAllRows' s
             return $ foldl f [] rows
             where f r (t:c:idc:[]) = (cTargetIds cf IM.! fromSql t,fromSql c,fromSql idc):r
                   f _ xs           = error $ "Unexpected data format." ++ show xs

recordF :: SQL -> Item Text -> PFEG ()
recordF sql i = do
    cf <- ask
    let item'    = indexItem (cUnigramIds cf) i
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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PFEG.Configuration
    ( ConfigError
    , Corpus
    , PFEGConfig(..)
    , ModeConfig(..)
    , configurePFEG
    , deinitialize ) where

import qualified Database.MongoDB as Mongo
import Control.Concurrent.Chan
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Ini.Types
import Data.Ini.Reader
import Data.Ini
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (hClose,openFile,IOMode(..),Handle)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Control.Monad.Error
import Control.Monad.Trans.State.Strict
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

data ConfigError = IOError FilePath
                 | OptionNotSet SectionName OptionName
                 | SectionNotPresent SectionName
                 | ParseError String
                 | GenericError String
                   deriving (Eq, Ord, Show)

instance Error ConfigError where
    noMsg  = GenericError "Oh shit!"
    strMsg = GenericError

type Name = String
type Corpus = (Name,FilePath)
type UnigramIDs = HashMap Text Int

data PFEGConfig = PFEGConfig
    { pfegMode   :: ModeConfig -- ^ Program mode specific configuration
    , unigramID  :: UnigramIDs -- ^ Unigram ids
    , statusLine :: Chan Int -- ^ Status update channel
    , contextDB  :: Connection -- ^ The connection to the main database
    , indexDB    :: Mongo.Pipe -- ^ Reverse index connection
    , targets    :: [Text] -- ^ Targets for this run
    , chunkSize  :: Int -- ^ Chunk size for the Iteratee
    }

data ModeConfig = Record { corpora   :: [Corpus] }
                | Match  { corpora   :: [Corpus]
                         , targetIDs :: IntMap Text
                         , majorityBaseline :: String
                         , resultLog :: Handle }
                | Index

newtype Configurator a = C { runC :: ErrorT ConfigError IO a }
                           deriving (Monad, MonadError ConfigError)

data RunMode = RunRecord | RunMatch | RunIndex
detectMode :: String -> Configurator RunMode
detectMode "match" = return RunMatch
detectMode "index" = return RunIndex
detectMode "record" = return RunRecord
detectMode x = throwError . GenericError $ "Unrecognized mode " ++ x

liftC :: IO a -> Configurator a
liftC m = C (lift m)

-- | Free all resources that were initialized earlier.
deinitialize :: PFEGConfig -> IO ()
deinitialize pfeg = do
    disconnect $ contextDB pfeg
    Mongo.close $ indexDB pfeg
    case pfegMode pfeg of m@Match {} -> hClose $ resultLog m
                          _ -> return ()

-- | Read configuration file and initialize all necessary data structures.
configurePFEG :: String -> FilePath -> IO (Either ConfigError PFEGConfig)
configurePFEG match f = do
    parseResult <- liftM parse (readFile f)
    case parseResult of
         (Left err)  -> return . Left . ParseError $ show err
         (Right cfg) -> runErrorT $ runC $ initialize match cfg

initialize :: String -> Config -> Configurator PFEGConfig
initialize modeString cfg = do
    uids  <- prepareUnigrams cfg
    ctxt  <- getValue cfg "databases" "contexts" >>= liftC . connectSqlite3
    csize <- readChunkSize cfg
    targs <- liftM splitAndStrip (getValue cfg "main" "targets")
    statC <- liftC newChan
    mongo <- getValue cfg "databases" "mongoDBHost"
    index <- liftC . Mongo.runIOE $ Mongo.connect (Mongo.host mongo)
    mode <- detectMode modeString
    runas <- case mode of
                  RunMatch -> do
                        test  <- getCorpusSet cfg "main" "teston"
                        resL  <- openHandle AppendMode cfg "main" "resultLog"
                        majB  <- getValue cfg "main" "majorityBaseline"
                        let tids = IM.fromList $ zip (mapMaybe (`M.lookup` uids) targs) targs
                        return Match { corpora   = test
                                     , targetIDs = tids
                                     , majorityBaseline = majB
                                     , resultLog = resL }
                  RunRecord -> do
                        train <- getCorpusSet cfg "main" "trainon"
                        return Record { corpora = train }
                  RunIndex -> return Index
    return PFEGConfig { pfegMode   = runas
                      , unigramID  = uids
                      , contextDB  = ctxt
                      , statusLine = statC
                      , indexDB    = index
                      , targets    = targs
                      , chunkSize  = csize }

splitAndStrip :: String -> [Text]
splitAndStrip = map (T.strip . T.pack) . splitOn ","

openHandle :: IOMode -> Config -> SectionName -> OptionName -> Configurator Handle
openHandle mode cfg sec opt = do
    fname <- getValue cfg sec opt
    liftC $ openFile fname mode

getCorpusSet :: Config -> SectionName -> OptionName -> Configurator [Corpus]
getCorpusSet cfg sec opt = do
    names <- liftM (map T.unpack . splitAndStrip) (getValue cfg sec opt)
    corps <- mapM (getValue cfg "data") names
    -- TODO: maybe we should check whether the corpora exist first.
    return $ zip names corps

readChunkSize :: Config -> Configurator Int
readChunkSize cfg = do
    val <- getValue cfg "main" "chunkSize"
    case reads val of
         [(i,"")] -> return i
         _        -> throwError $ GenericError ("Unable to parse " ++ val ++ " as integer.")

prepareUnigrams :: Config -> Configurator UnigramIDs
prepareUnigrams cfg = do
    val  <- getValue cfg "databases" "unigrams"
    conn <- liftC $ connectSqlite3 val
    stmt <- liftC $ prepare conn "SELECT f,id FROM unigrams"
    uids <- liftC $ execStateT (cacheHash stmt) M.empty
    liftC $ disconnect conn
    return uids

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

getValue :: Config -> SectionName -> OptionName -> Configurator OptionValue
getValue cfg sec opt | hasSection sec cfg = case getOption sec opt cfg of
                               (Just val) -> return val
                               Nothing    -> throwError $ OptionNotSet sec opt
                     | otherwise          = throwError $ SectionNotPresent sec


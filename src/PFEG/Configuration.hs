{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PFEG.Configuration
    ( ConfigError
    , PFEGConfig(..)
    , configurePFEG
    , deinitialize ) where

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
    { pfegMode   :: ModeConfig
    , unigramID  :: UnigramIDs -- ^ Unigram ids
    , contextDB  :: Connection -- ^ The connection to the main database
    , targets    :: [Text] -- ^ Targets for this run
    , chunkSize  :: Int -- ^ Chunk size for the Iteratee
    }

data ModeConfig = Record { trainingC :: [Corpus] }
                | Match  { testingC  :: [Corpus]
                         , targetIDs :: IntMap Text
                         , resultLog :: Handle }

newtype Configurator a = C { runC :: ErrorT ConfigError IO a }
                           deriving (Monad, MonadError ConfigError)

liftC :: IO a -> Configurator a
liftC m = C (lift m)

-- | Free all resources that were initialized earlier.
deinitialize :: PFEGConfig -> IO ()
deinitialize pfeg = do
    disconnect $ contextDB pfeg
    case pfegMode pfeg of m@(Match _ _ _) -> hClose $ resultLog m
                          (Record _)      -> return ()

-- | Read configuration file and initialize all necessary data structures.
configurePFEG :: Bool -> FilePath -> IO (Either ConfigError PFEGConfig)
configurePFEG match f = do
    parseResult <- liftM parse (readFile f)
    case parseResult of
         (Left err)  -> return . Left . ParseError $ show err
         (Right cfg) -> runErrorT $ runC $ initialize match cfg

initialize :: Bool -> Config -> Configurator PFEGConfig
initialize match cfg = do
    uids  <- prepareUnigrams cfg
    ctxt  <- getValue cfg "databases" "contexts" >>= liftC . connectSqlite3 
    csize <- readChunkSize cfg
    targs <- liftM splitAndStrip (getValue cfg "main" "targets")
    runas <- if match
               then (do test  <- getCorpusSet cfg "main" "teston"
                        resL  <- openHandle AppendMode cfg "main" "resultLog"
                        let tids = IM.fromList $ zip (mapMaybe (`M.lookup` uids) targs) targs
                        return Match { testingC = test
                                     , targetIDs = tids
                                     , resultLog = resL })
               else (do train <- getCorpusSet cfg "main" "trainon"
                        return Record { trainingC = train })
    return PFEGConfig { pfegMode  = runas
                      , unigramID = uids
                      , contextDB = ctxt
                      , targets   = targs
                      , chunkSize = csize }

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


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PFEG.Configuration
    ( ConfigError
    , Corpus
    , UnigramIDs
    , PFEGConfig(..)
    , ModeConfig(..)
    , configurePFEG
    , deinitialize ) where

import Control.Concurrent (forkIO,killThread)
import PFEG.Common
import Control.Concurrent.Chan
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Ini.Types
import Data.Ini.Reader
import Data.Ini
import Database.HDBC
import Database.HDBC.PostgreSQL
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
    , statusLine :: Chan Int -- ^ Status update channel
    , database   :: Connection -- ^ The connection to the main database
    , targets    :: [Text] -- ^ Targets for this run
    , chunkSize  :: Int -- ^ Chunk size for the Iteratee
    }

data ModeConfig = Record { corpora   :: [Corpus]
                         , unigramIDs  :: UnigramIDs } -- ^ Unigram ids
                | Match  { corpora   :: [Corpus]
                         , targetIDs :: IntMap Text
                         , unigramIDs  :: UnigramIDs  -- ^ Unigram ids
                         , majorityBaseline :: String
                         , resultLog :: Handle }
                | Unigrams { corpora :: [Corpus] }

newtype Configurator a = C { runC :: ErrorT ConfigError IO a }
                           deriving (Monad, MonadError ConfigError, MonadIO)

data RunMode = RunRecord | RunMatch | RunUnigrams
detectMode :: String -> Configurator RunMode
detectMode "match" = return RunMatch
detectMode "unigrams" = return RunUnigrams
detectMode "record" = return RunRecord
detectMode x = throwError . GenericError $ "Unrecognized mode " ++ x

liftC :: IO a -> Configurator a
liftC m = C (lift m)

-- | Free all resources that were initialized earlier.
deinitialize :: PFEGConfig -> IO ()
deinitialize pfeg = do
    disconnect $ database pfeg
    case pfegMode pfeg of m@Match{} -> hClose $ resultLog m
                          _ -> return ()

-- | Read configuration file and initialize all necessary data structures.
configurePFEG :: String -> FilePath -> IO (Either ConfigError PFEGConfig)
configurePFEG match f = do
    parseResult <- liftM parse (readFile f)
    case parseResult of
         (Left err)  -> return . Left . ParseError $ show err
         (Right cfg) -> runErrorT $ runC $ initialize match cfg

getPostgresOpts :: Config -> Configurator [(String,String)]
getPostgresOpts cfg = do
    host   <- getValue cfg "database" "host"
    user   <- getValue cfg "database" "user"
    dbName <- getValue cfg "database" "dbName"
    return [ ("host" , host) , ("user",user) , ("dbname",dbName)  ]

initialize :: String -> Config -> Configurator PFEGConfig
initialize modeString cfg = do
    opts <- getPostgresOpts cfg
    db  <- liftC . connectPostgreSQL $ unwords [ k ++ "=" ++ v | (k,v) <- opts ]
    csize <- readChunkSize cfg
    targs <- liftM splitAndStrip (getValue cfg "main" "targets")
    statC <- liftC newChan
    mode <- detectMode modeString
    runas <- case mode of
                  RunMatch -> do
                        uids  <- prepareUnigrams db
                        test  <- getCorpusSet cfg "main" "teston"
                        resL  <- openHandle AppendMode cfg "main" "resultLog"
                        majB  <- getValue cfg "main" "majorityBaseline"
                        let tids = IM.fromList $ zip (mapMaybe (`M.lookup` uids) targs) targs
                        return Match { corpora   = test
                                     , targetIDs = tids
                                     , majorityBaseline = majB
                                     , resultLog = resL
                                     , unigramIDs = uids }
                  RunRecord -> do
                        uids  <- prepareUnigrams db
                        train <- getCorpusSet cfg "main" "trainon"
                        return Record { corpora = train, unigramIDs = uids }
                  RunUnigrams -> do
                        train <- getCorpusSet cfg "main" "trainon"
                        test  <- getCorpusSet cfg "main" "teston"
                        return Unigrams { corpora = train ++ test }
    liftIO $ putStrLn "Done."
    return PFEGConfig { pfegMode   = runas
                      , database   = db
                      , statusLine = statC
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

prepareUnigrams :: Connection -> Configurator UnigramIDs
prepareUnigrams conn =
    liftC $ do rows <- quickQuery' conn "SELECT count(*) FROM unigrams" []
               (finalize,logChan) <- case rows of
                   [[x]] -> do
                        logChan <- newChan
                        threadID <- forkIO $ logger (fromSql x) logChan
                        return (\a -> killThread threadID >> return a,logChan)
                   xs -> error $ "Failed to obtain count from unigrams " ++ show xs
               stmt <- prepare conn "SELECT form,id FROM unigrams"
               execStateT (cacheHash logChan stmt) (M.empty,0) >>= finalize.fst

cacheHash :: Chan Int -> Statement -> StateT (UnigramIDs,Int) IO ()
cacheHash logChan s = liftIO (void $ execute s []) >> fetchAll
    where fetchAll = do
          row <- liftIO $ fetchRow s
          case row of
               Nothing       -> return ()
               Just (f:i:[]) -> do
                   (m,c) <- get
                   when (c `mod` 10000 == 0) (liftIO $ writeChan logChan c)
                   put (M.insert (fromSql f) (fromSql i) m,c+1)
                   fetchAll
               _             -> fail "Malformed result in unigrams."

getValue :: Config -> SectionName -> OptionName -> Configurator OptionValue
getValue cfg sec opt | hasSection sec cfg = case getOption sec opt cfg of
                               (Just val) -> return val
                               Nothing    -> throwError $ OptionNotSet sec opt
                     | otherwise          = throwError $ SectionNotPresent sec


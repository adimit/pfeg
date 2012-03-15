{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PFEG.Configuration
    ( ConfigError
    , Corpus
    , PFEGConfig(..)
    , ModeConfig(..)
    , configurePFEG
    , deinitialize ) where

import Text.Search.Sphinx.Types (Sort(..),GroupByFunction(..),MatchMode(..))
import qualified Text.Search.Sphinx as S
import Control.Concurrent.Chan
import Data.Ini.Types
import Data.Ini.Reader
import Data.Ini
import Database.HDBC
import Database.HDBC.MySQL
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (hClose,openFile,IOMode(..),Handle)
import Control.Monad.Error
import Data.List.Split (splitOn)

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

data PFEGConfig = PFEGConfig
    { pfegMode   :: ModeConfig -- ^ Program mode specific configuration
    , statusLine :: Chan Int -- ^ Status update channel
    , database   :: Connection -- ^ The connection to the main database
    , targets    :: [Text] -- ^ Targets for this run
    , majorityBaseline :: String
    , chunkSize  :: Int -- ^ Chunk size for the Iteratee
    }

data ModeConfig = Record { corpora   :: [Corpus] }
                | Match  { corpora   :: [Corpus]
                         , searchConf:: S.Configuration
                         , resultLog :: Handle }
                | Predict { corpora  :: [Corpus] }

newtype Configurator a = C { runC :: ErrorT ConfigError IO a }
                           deriving (Monad, MonadError ConfigError, MonadIO)

data RunMode = RunRecord | RunMatch | RunPredict
detectMode :: String -> Configurator RunMode
detectMode "match" = return RunMatch
detectMode "record" = return RunRecord
detectMode "predict" = return RunPredict
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

getMySQLInfo :: Config -> Configurator MySQLConnectInfo
getMySQLInfo cfg = do
    host   <- getValue cfg "database" "host"
    user   <- getValue cfg "database" "user"
    dbName <- getValue cfg "database" "dbName"
    return defaultMySQLConnectInfo { mysqlHost = host, mysqlUser = user, mysqlDatabase = dbName }

initialize :: String -> Config -> Configurator PFEGConfig
initialize modeString cfg = do
    connInfo <- getMySQLInfo cfg
    db  <- liftC . withRTSSignalsBlocked . connectMySQL $ connInfo
    csize <- readChunkSize cfg
    targs <- liftM splitAndStrip (getValue cfg "main" "targets")
    statC <- liftC newChan
    majB  <- getValue cfg "main" "majorityBaseline"
    mode <- detectMode modeString
    runas <- case mode of
                  RunMatch -> do
                        test  <- getCorpusSet cfg "main" "teston"
                        resL  <- openHandle AppendMode cfg "main" "resultLog"
                        shost <- getValue cfg "sphinx" "host"
                        sport <- liftM read $ getValue cfg "sphinx" "port"
                        return Match { corpora          = test
                                     , searchConf       = S.defaultConfig
                                        { S.host        = shost
                                        , S.port        = sport
                                        , S.groupByFunc = Attr
                                        , S.groupBy     = "target"
                                        , S.sort        = AttrDesc
                                        , S.sortBy      = "target"
                                        , S.mode        = Extended }
                                     , resultLog        = resL }
                  RunRecord -> do
                        train <- getCorpusSet cfg "main" "trainon"
                        return Record { corpora = train }
                  RunPredict -> do
                        predict <- getCorpusSet cfg "main" "predicton"
                        return Predict { corpora = predict }
    liftIO $ putStrLn "Done."
    return PFEGConfig { pfegMode   = runas
                      , database   = db
                      , statusLine = statC
                      , targets    = targs
                      , majorityBaseline = majB
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

getValue :: Config -> SectionName -> OptionName -> Configurator OptionValue
getValue cfg sec opt | hasSection sec cfg = case getOption sec opt cfg of
                               (Just val) -> return val
                               Nothing    -> throwError $ OptionNotSet sec opt
                     | otherwise          = throwError $ SectionNotPresent sec


{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module PFEG.Configuration
    ( ConfigError
    , Corpus
    , PFEGConfig(..)
    , ModeConfig(..)
    , configurePFEG
    , deinitialize ) where

import Text.Search.Sphinx.Types (MatchMode(..))
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
import qualified Text.Search.Sphinx.ExcerptConfiguration as Ex
import qualified PFEG.Pattern as Pat
import Data.Either
import qualified Text.Parsec as Parsec
import Data.Text.ICU.Convert

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
    { pfegMode         :: ModeConfig -- ^ Program mode specific configuration
    , statusLine       :: Chan Int -- ^ Status update channel
    , debugLogHandle   :: Handle -- ^ Debugging log file
    , database         :: Connection -- ^ The connection to the main database
    , corpusConverter  :: Converter -- ^ Text.ICU input encoding converter
    , targets          :: [Text] -- ^ Targets for this run
    , majorityBaseline :: String
    , sphinxIndex      :: String
    , chunkSize        :: Int -- ^ Chunk size for the Iteratee
    , matchPatterns    :: [Pat.MatchPattern] }

data ModeConfig = Record { corpora   :: [Corpus] }
                | Match  { corpora   :: [Corpus]
                         , searchConf:: S.Configuration
                         , exConf    :: Ex.ExcerptConfiguration
                         , resultLog :: Handle }
                | Predict { corpora  :: [Corpus]
                          , searchConf:: S.Configuration
                          , exConf    :: Ex.ExcerptConfiguration
                          , resultLog :: Handle }

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
    liftC $ prepare db "SET NAMES 'utf8'" >>= executeRaw
    csize <- readChunkSize cfg
    targs <- liftM splitAndStrip (getValue cfg "main" "targets")
    statC <- liftC newChan
    majB  <- getValue cfg "main" "majorityBaseline"
    debL  <- openHandle AppendMode cfg "main" "debugLog"
    mode <- detectMode modeString
    shost <- getValue cfg "sphinx" "host"
    sport <- liftM read $ getValue cfg "sphinx" "port"
    sindex <- getValue cfg "sphinx" "index"
    encoding <- getValue cfg "data" "encoding"
    conv <- liftC $ open encoding Nothing
    pats <- getPatterns cfg "patterns" "patterns"
    runas <- case mode of
      RunMatch -> do
            test  <- getCorpusSet cfg "tasks" "match"
            resL  <- openHandle AppendMode cfg "main" "resultLog"
            return Match { corpora    = test
                         , exConf     = defaultExcerptConf shost sport
                         , searchConf = defaultSearchConf shost sport
                         , resultLog  = resL }
      RunRecord -> do
            train <- getCorpusSet cfg "tasks" "record"
            return Record { corpora = train }
      RunPredict -> do
            predict <- getCorpusSet cfg "tasks" "predict"
            resL <- openHandle AppendMode cfg "main" "predictLog"
            return Predict { corpora    = predict
                           , exConf     = defaultExcerptConf shost sport
                           , searchConf = defaultSearchConf shost sport
                           , resultLog  = resL }
    let config = PFEGConfig { pfegMode         = runas
                            , database         = db
                            , statusLine       = statC
                            , debugLogHandle   = debL
                            , sphinxIndex      = sindex
                            , targets          = targs
                            , corpusConverter  = conv
                            , majorityBaseline = majB
                            , matchPatterns    = pats
                            , chunkSize        = csize }
    liftIO $ do putStrLn "Done."
                printConfig config
    return config

printConfig :: PFEGConfig -> IO ()
printConfig c =
    putStr $ "PFEG Configuration:\n" ++
             "\tMajority baseline: " ++ majorityBaseline c ++ "\n\
             \\tTargets: " ++ unwords (map T.unpack (targets c)) ++ "\n\
             \\tPatterns: " ++ (unlines . addSpaces) (map (unwords . map show) (makeBits 3 $ matchPatterns c)) ++ "\n\
             \\t" ++ showMode (pfegMode c)
    where addSpaces :: [String] -> [String]
          addSpaces [] = []
          addSpaces (h:t) = h:map ("\t          "++) t
          makeBits :: Int -> [a] -> [[a]]
          makeBits _ [] = []
          makeBits n l  = let (h,t) = splitAt n l
                          in h:makeBits n t

showMode :: ModeConfig -> String
showMode c = mode ++ "Corpora:\n" ++ unlines (map (('\t':).snd) (corpora c))
    where mode = case c of Record  {} -> "PFEG is in RECORD mode\n"
                           Match   {} -> "PFEG is in MATCH mode\n"
                           Predict {} -> "PFEG is in PREDICT mode\n"

defaultExcerptConf :: String -> Int -> Ex.ExcerptConfiguration
defaultExcerptConf shost sport = Ex.altConfig
    { Ex.host        = shost
    , Ex.port        = sport
    , Ex.beforeMatch = "{"
    , Ex.afterMatch  = "}" }

defaultSearchConf :: String -> Int -> S.Configuration
defaultSearchConf shost sport = S.defaultConfig
     { S.host        = shost
     , S.port        = sport
     , S.mode        = Extended }

splitAndStrip :: String -> [Text]
splitAndStrip = map (T.strip . T.pack) . splitOn ","

openHandle :: IOMode -> Config -> SectionName -> OptionName -> Configurator Handle
openHandle mode cfg sec opt = do
    fname <- getValue cfg sec opt
    liftC $ openFile fname mode

getPatterns :: Config -> SectionName -> OptionName -> Configurator [Pat.MatchPattern]
getPatterns cfg sec name = do
    pLocations <- liftM (map T.unpack . splitAndStrip) $ getValue cfg sec name
    liftM concat (forM pLocations getPattern)
    where getPattern n = do
              ps <- liftM splitAndStrip (getValue cfg sec n)
              let (errs,pats) = partitionEithers $ map (Parsec.parse Pat.parsePattern sec) ps
              forM_ errs $ \err -> liftC (putStrLn $ "***\nWARNING: " ++ show err++"\n***")
              return pats

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


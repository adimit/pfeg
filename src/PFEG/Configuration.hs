{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module PFEG.Configuration
    ( ConfigError
    , Regexes(..)
    , Corpus
    , PFEGConfig(..)
    , ModeConfig(..)
    , configurePFEG
    , deinitialize ) where

import System.Directory
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
import qualified PFEG.Pattern as Pat
import Data.Either
import qualified Text.Parsec as Parsec
import Data.Text.ICU.Convert
import Data.Text.ICU

data Regexes = Regexes
     { numeralRegex :: Regex -- ^ when tagged CARD and matching this, tokens are left as is
     , dateRegex    :: Regex -- ^ when tagged CARD and matching this, surface is DATE
     , timeRegex    :: Regex -- ^ when tagged CARD and matching this, surface is TIME
     , cardTag :: Text -- ^ The pos tag that represents cardinalities
     }

data ConfigError = IOError FilePath
                 | OptionNotSet SectionName OptionName
                 | SectionNotPresent SectionName
                 | ParseError String
                 | GenericError String
                 | FileNotFoundError FilePath
                   deriving (Eq, Ord, Show)

instance Error ConfigError where
    noMsg  = GenericError "Oh shit!"
    strMsg = GenericError

type Name = String
type Corpus = (Name,FilePath)

data PFEGConfig = PFEGConfig
    { pfegMode         :: ModeConfig -- ^ Program mode specific configuration
    , statusLine       :: Chan Int -- ^ Status update channel
    , database         :: Connection -- ^ The connection to the main database
    , corpusConverter  :: Converter -- ^ Text.ICU input encoding converter
    , targets          :: [Text] -- ^ Targets for this run
    , majorityBaseline :: Text
    , sphinxIndex      :: Text
    , searchConf       :: S.Configuration
    , cardRegexes      :: Regexes
    , debugLog         :: Handle -- ^ Write debug information to this log
    , chunkSize        :: Int -- ^ Chunk size for the Iteratee
    , matchPatterns    :: [Pat.MatchPattern] }

data ModeConfig = Record  { corpora :: [Corpus] }
                | Predict { corpus  :: Corpus }
                | Learn   { corpus  :: Corpus, statLog :: Handle }

newtype Configurator a = C { runC :: ErrorT ConfigError IO a }
                           deriving (Monad, MonadError ConfigError, MonadIO)

data RunMode = RunRecord | RunLearn | RunPredict
detectMode :: String -> Configurator RunMode
detectMode "learn" = return RunLearn
detectMode "record" = return RunRecord
detectMode "predict" = return RunPredict
detectMode x = throwError . GenericError $ "Unrecognized mode " ++ x

liftC :: IO a -> Configurator a
liftC m = C (lift m)

-- | Free all resources that were initialized earlier.
deinitialize :: PFEGConfig -> IO ()
deinitialize pfeg = do
    disconnect $ database pfeg
    hClose $ debugLog pfeg
    case pfegMode pfeg of m@Learn{} -> hClose $ statLog m
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
    cardPOS <- getValue cfg "data" "cardtag"
    dateRE <- parseRegex =<< getValue cfg "data" "dates"
    timeRE <- parseRegex =<< getValue cfg "data" "times"
    numrRE <- parseRegex =<< getValue cfg "data" "numerals"
    shost <- getValue cfg "sphinx" "host"
    sport <- liftM read $ getValue cfg "sphinx" "port"
    sindex <- getValue cfg "sphinx" "index"
    encoding <- getValue cfg "data" "encoding"
    conv <- liftC $ open encoding Nothing
    pats <- getPatterns cfg "patterns" "patterns"
    runas <- case mode of
      RunLearn -> do
            corp <- getValue cfg "tasks" "learn" >>= getCorpus cfg
            resL  <- openHandle AppendMode cfg "main" "statLog"
            return Learn { corpus = corp, statLog = resL }
      RunRecord -> do
            train <- getCorpusSet cfg "tasks" "record"
            return Record { corpora = train }
      RunPredict -> do
            corp <- getValue cfg "tasks" "learn" >>= getCorpus cfg
            return Predict { corpus = corp }
    let config = PFEGConfig { pfegMode         = runas
                            , database         = db
                            , statusLine       = statC
                            , searchConf = defaultSearchConf shost sport 
                            , cardRegexes      = Regexes { numeralRegex = numrRE
                                                         , dateRegex = dateRE
                                                         , timeRegex = timeRE
                                                         , cardTag = T.pack cardPOS }
                            , debugLog         = debL
                            , sphinxIndex      = T.pack sindex
                            , targets          = targs
                            , corpusConverter  = conv
                            , majorityBaseline = T.pack majB
                            , matchPatterns    = pats
                            , chunkSize        = csize }
    liftIO $ do putStrLn "Done."
                printConfig config
    return config

printConfig :: PFEGConfig -> IO ()
printConfig c =
    putStr $ "PFEG Configuration:\n" ++
             "\tMajority baseline: " ++ T.unpack (majorityBaseline c) ++ "\n\
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
showMode Record  { corpora = cs } = "PFEG is in RECORD mode\n"  ++ unlines (map (('\t':).snd) cs) ++ "\n"
showMode Learn   { corpus = c }   = "PFEG is in LEARN mode\n"   ++ '\t':snd c                     ++ "\n"
showMode Predict { corpus = c }   = "PFEG is in PREDICT mode\n" ++ '\t':snd c                     ++ "\n"

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
    mapM (getCorpus cfg) names

getCorpus :: Config -> String -> Configurator Corpus
getCorpus cfg name = do
    corps <- getValue cfg "data" name
    isfr <- liftIO $ isFileReadable corps
    if isfr then return (name,corps) else throwError $ FileNotFoundError corps

isFileReadable :: FilePath -> IO Bool
isFileReadable f = liftM2 (&&) (doesFileExist f) (liftM readable $ getPermissions f)

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

parseRegex :: OptionValue -> Configurator Regex
parseRegex = fromEither . regex' [] . T.pack
    where fromEither (Right r)  = return r
          fromEither (Left err) = throwError . ParseError $ 
            "Failed Regex parse: " ++ show err ++ "\nCheck " ++ url ++ " for regex syntax."
          url = "http://userguide.icu-project.org/strings/regexp"

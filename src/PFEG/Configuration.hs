module PFEG.Configuration where

import Data.Ini.Types
import Data.Ini.Reader
import Data.Ini
import Database.HDBC.Sqlite3
import Data.Text (Text)
import System.IO (Handle)
import Data.HashMap.Strict (HashMap)
import Control.Monad (liftM)

type Name = String
type Corpus = (Name,FilePath)
type UnigramIDs = HashMap Text Int

data PFEGConfig = PFEGConfig
    { trainingC  :: [Corpus]
    , testingC   :: [Corpus]
    , unigramID  :: UnigramIDs
    , contextDB  :: Connection
    , targets    :: [Text]
    , chunkSize  :: Int
    , resultLog  :: Handle }

configurePFEG :: FilePath -> IO (Either String PFEGConfig)
configurePFEG f = liftM parse (readFile f) >>= initialize

initialize :: IniParseResult Config -> IO (Either String PFEGConfig)
initialize (Right cfg) = do
    contextDB' <- connectSqlite3 undefined
    return undefined
initialize (Left err) = return (Left $ show err)

configOrDie :: Config -> SectionName -> OptionName -> String -> Either String OptionValue
configOrDie cfg sec opt err = case getOption sec opt cfg of
                                   (Just val) -> Right val
                                   Nothing    -> Left err

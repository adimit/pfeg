module PFEG.Configuration where

import Data.Ini
import Database.HDBC.Sqlite3
import Data.Text (Text)
import System.IO (Handle)

type Name = String
type Corpus = (Name,FilePath)

data PFEGConfig = PFEGConfig
    { trainingC  :: [Corpus]
    , testingC   :: [Corpus]
    , unigramDB  :: Connection
    , contextDB  :: Connection
    , targets    :: [Text]
    , chunkSize  :: Int
    , resultLog  :: Handle }

module PFEG.Common
    ( -- * Types
      Wordcounts
      -- * Configuration
    , Configuration(..)
    , standardConfig
      -- * Measuring execution times
    , doTimed
    , doTimed_
    ) where

import Data.Time.Clock
import qualified Data.HashMap.Strict as T
import qualified Data.Text as X

type Wordcounts = T.HashMap X.Text Int

data Configuration = Config { unigramTable :: String }

standardConfig :: Configuration
standardConfig = Config
    { unigramTable = "unigrams" }

doTimed :: IO a -> IO (a,NominalDiffTime)
doTimed f = do
    start  <- getCurrentTime
    result <- f
    end    <- getCurrentTime
    return (result, end `diffUTCTime` start)

doTimed_ :: IO a -> IO NominalDiffTime
doTimed_ f = fmap snd (doTimed f)

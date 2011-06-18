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

import PFEG.Types
import Data.Time.Clock
import qualified Data.HashMap.Strict as T
import qualified Data.Text as X

standardConfig :: Configuration
standardConfig = Config { lemmaTable   = "lemmas"
                        , posTable     = "pos"
                        , unigramTable = "unigrams"
                        , targets      = [ "in" , "von" , "mit", "für", "im"
                                         , "auf", "nach", "an" , "aus", "am"] }

doTimed :: IO a -> IO (a,NominalDiffTime)
doTimed f = do
    start  <- getCurrentTime
    result <- f
    end    <- getCurrentTime
    return (result, end `diffUTCTime` start)

doTimed_ :: IO a -> IO NominalDiffTime
doTimed_ f = fmap snd (doTimed f)

-- {-# LANGUAGE TupleSections,BangPatterns #-}
module Main where

import Data.List (intercalate)
import Control.Monad.Reader
import Control.Monad.State
import GHC.IO.Handle (Handle)
import Data.Maybe (catMaybes)

import Data.Text (Text)
import qualified Data.Text as T

import PFEG.Context
import PFEG.Common hiding (Configuration)

data LogState = LogState { currentItem :: Int }

data Configuration = Configuration
    { testShard :: [Int]
    , targets   :: [Text]
    , hashtable :: String
    , ctxttable :: String }

data MatchMode = P | L | S

type SQLString = String
type Result = [(Int,Text)] -- list of possible predictions with associated counts.

match :: Item Text (Context Text) -> [MatchMode] -> ReaderT Configuration IO Result
match = undefined

-- | Given a list of @MatchMode@s and an item, make the appropriate SQL string
-- to query the hash DB with.
sqlHash :: Item Text (Context Text) -> [Maybe MatchMode] -> Reader Configuration SQLString
sqlHash (Item (Context pI) (Context lI) (Context sI) _t) mm = do
    cf <- ask
    return $ "SELECT h FROM " ++ hashtable cf ++ " WHERE " ++ intercalate " AND " pattern
    where pattern           = catMaybes $ zipWith3 mmSelect mm (zip3 pI lI sI) ([1..]::[Int])
          f c s n           = Just $ c:show n ++ " == " ++ T.unpack s
          mmSelect (Just P) = f 'p'.fst3
          mmSelect (Just L) = f 'l'.snd3
          mmSelect (Just S) = f 's'.trd3
          mmSelect Nothing  = \_ _ -> Nothing

logResult :: Handle -> Item Text (Context Text) -> Result -> StateT LogState IO ()
logResult _ _ _ = undefined

main :: IO ()
main = undefined

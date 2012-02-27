{-# LANGUAGE FlexibleContexts #-}
module PFEG.SQL
    ( item2SQL
    , item2postgresArrays
      -- * Upserts
    , upsertUnigram
    , upsertRecord
    ) where

import Database.HDBC

import PFEG.Common
import PFEG.Context

import Data.Text (Text)
import qualified Data.Text as T

import Data.List (intercalate)
import Data.Maybe (catMaybes)

import Prelude hiding (null)

upsertRecord :: String
upsertRecord = "SELECT records_upsert(?,?,?)"

upsertUnigram :: String
upsertUnigram = "SELECT unigram_upsert(?,?)"

item2SQLp :: MatchPattern -> Item i -> [i]
item2SQLp (MatchPattern mm) Item{ pItem = Context pI, lItem = Context lI, sItem = Context sI}  =
    catMaybes $ zipWith ms mm (zip3 pI lI sI)
    where ms (Just P) = Just . fst3
          ms (Just L) = Just . snd3
          ms (Just S) = Just . trd3
          ms Nothing  = const Nothing

item2postgresArrays :: (a -> String) -> Item a -> MatchPattern -> (SqlValue,SqlValue)
item2postgresArrays show' i mm =
    (toPostgresArray show $ matchPositions mm,toPostgresArray show' $ item2SQLp mm i)

matchPositions :: MatchPattern -> [Int]
matchPositions (MatchPattern mm) = catMaybes $ zipWith f mm [1..]
    where f (Just S) x = Just x
          f (Just L) x = Just $ 6+x
          f (Just P) x = Just $ 12+x
          f Nothing  _ = Nothing

toPostgresArray :: (a -> String) -> [a] -> SqlValue
toPostgresArray show' as = toSql $ '{' : intercalate "," (map show' as) ++ "}"

-- Convert an item's payload to Postgres Array String representation *without* the target.
item2SQL :: Item Text -> SqlValue
item2SQL Item { pItem = (Context ps), lItem = (Context ls), sItem = (Context ss) } =
    toPostgresArray T.unpack (ss++ls++ps)

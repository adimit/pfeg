{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module PFEG.SQL
    ( item2SQL
    , item2postgresArrays
      -- * Statements
    , upsertUnigram
    , upsertRecord
    , queryDatabase
    , cleanup
    ) where

import Database.HDBC

import PFEG.Types
import PFEG.Context

import Data.Text (Text)
import qualified Data.Text as T

import Data.List (mapAccumL,intercalate)
import Data.Maybe (catMaybes)

import Prelude hiding (null)

-- fiendishly ugly, but it gets the job done.
groupByJust :: [Maybe a] -> [[a]]
groupByJust l = let (rest,list) = mapAccumL f [] l
                in  catMaybes list ++ case [catMaybes rest] of
                                          [[]] -> []
                                          xs -> xs
                where f :: [Maybe a] -> Maybe a -> ([Maybe a],Maybe [a])
                      f [] Nothing = ([],Nothing)
                      f acc Nothing = ([],Just $ catMaybes acc)
                      f acc a@Just{} = (acc++[a],Nothing)

-- | Call records_upsert(TEXT,INTEGER,TEXT[])
upsertRecord :: String
upsertRecord = "SELECT records_upsert(?,?,?)"

-- | Call unigram_upsert(TEXT,INTEGER)
upsertUnigram :: String
upsertUnigram = "SELECT unigram_upsert(?,?)"

-- | Call match_function(INTEGER[],TEXT[])
queryDatabase :: String
queryDatabase = "SELECT query_records(?,?)"

cleanup :: String
cleanup = "VACUUM ANALYZE"

item2SQLp :: MatchPattern -> Item i -> [[i]]
item2SQLp mm Item{ pItem = Context pI, lItem = Context lI, sItem = Context sI}  =
    groupByJust $ zipWith ($) (matchPatternToPositional mm) (sI++lI++pI)

-- | Prepare an item and a certain matchmode for @queryDatabase@
item2postgresArrays :: (a -> String) -> Item a -> MatchPattern -> (SqlValue,SqlValue)
item2postgresArrays show' i mm =
    ( toPostgresArray show . positionalToRanges $ matchPatternToPositional mm
    , toPostgresArray fromSql (map (toPostgresArray show') (item2SQLp mm i)))

data Range = !Int :-: !Int
instance Show Range where
    show (a :-: b) = show a ++ ":" ++ show b

matchPatternToPositional :: MatchPattern -> Positional a
matchPatternToPositional (MatchPattern mm) = 
    let (a,b,c) = unzip3 $ map f (take 6 $ mm ++ repeat Nothing) in a++b++c
    where f (Just P) = (nothing,nothing,Just)
          f (Just L) = (nothing,Just,nothing)
          f (Just S) = (Just,nothing,nothing)
          f Nothing  = (nothing,nothing,nothing)
          nothing = const Nothing

positionalToRanges :: Positional Int -> [Range]
positionalToRanges p = map (\x -> head x :-: last x) . groupByJust $ zipWith ($) p [1..]

type Positional a = [a -> Maybe a]

toPostgresArray :: (a -> String) -> [a] -> SqlValue
toPostgresArray show' as = toSql $ '{' : intercalate "," (map show' as) ++ "}"

-- Convert an item's payload to Postgres Array String representation *without* the target.
item2SQL :: Item Text -> SqlValue
item2SQL Item { pItem = (Context ps), lItem = (Context ls), sItem = (Context ss) } =
    toPostgresArray T.unpack (ss++ls++ps)

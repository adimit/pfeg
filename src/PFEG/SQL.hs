{-# LANGUAGE FlexibleContexts #-}
module PFEG.SQL
    ( contexts2SQL
    , item2SQL
      -- * Hash statements
    , insertHash
    , insertWithHash
    , updateWithHash
    ) where

import Database.HDBC

import PFEG.Context

import Data.List (intercalate,intersperse)

import Data.Convertible.Base (Convertible)

import Prelude hiding (null)

insertHash, updateWithHash, insertWithHash :: String
insertHash = "INSERT OR IGNORE INTO hashes (h,"++contextNames++") VALUES (?,"++ questionmarks 18++")"
    where contextNames = intercalate "," $ map (`commas` [1..6]) "slp"
updateWithHash = "UPDATE ctxt SET c=c+1 WHERE h==? AND i==? AND t==?"
insertWithHash = "INSERT INTO ctxt (h,i,t,c) VALUES (?,?,?,1)"

commas :: Char -> [Int] -> String
commas c = intercalate "," . letters c

letters :: Char -> [Int] -> [String]
letters c = map ((c:) . show)

questionmarks :: Int -> String
questionmarks range = intersperse ',' $ replicate range '?'

item2SQL :: (Convertible a SqlValue, Convertible i SqlValue) => Int -> Item i (Context a) -> [SqlValue]
item2SQL i (Item (Context ps) (Context ls) (Context ss) t) =
    toSql i: toSql t: (map toSql ss ++ map toSql ls ++ map toSql ps)

contexts2SQL :: (Convertible a SqlValue) => Item i (Context a) -> [SqlValue]
contexts2SQL (Item (Context a) (Context b) (Context c) _t) =
    map toSql (c++b++a)

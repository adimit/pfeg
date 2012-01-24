{-# LANGUAGE FlexibleContexts #-}
module PFEG.SQL
    ( contexts2SQL
    , item2SQL
      -- * Hash statements
    , insertHash
    , insertWithHash
    , updateWithHash
      -- * New SQL statements
    , updateSQL
    , insertCtxtSQL
    , insertTargetSQL
    ) where

import Database.HDBC

import PFEG.Context

import Data.List (intercalate,intersperse)

import Data.Convertible.Base (Convertible)

import Prelude hiding (null)

insertHash, updateWithHash, insertWithHash :: String
insertHash = "INSERT OR IGNORE INTO hash (h,"++contextNames++") VALUES (?,"++ questionmarks 18++")"
    where contextNames = intercalate "," $ map (`commas` [1..6]) "slp"
updateWithHash = "UPDATE ctxt SET c=c+1 WHERE h==? AND i==? AND t==?"
insertWithHash = "INSERT INTO ctxt (h,i,t,c) VALUES (?,?,?,1)"


updateSQL, insertCtxtSQL, insertTargetSQL, selectSubquerySQL :: String
updateSQL = "UPDATE targets SET c=c+1 WHERE id="++selectSubquerySQL++" AND t==? AND i==?"
insertCtxtSQL = "INSERT or IGNORE INTO ctxt ("++contextNames++") VALUES ("++questionmarks 18++")"
    where contextNames = intercalate "," $ map (`commas` [1..6]) "slp"
insertTargetSQL = "INSERT INTO targets (id,t,i,c) VALUES ("++selectSubquerySQL++",?,?,1)"
selectSubquerySQL = "(SELECT id FROM ctxt WHERE "++ cn ++")"
    where cn = intercalate " AND " $ map (\c -> intercalate " AND " $ map (++"==?") (letters c [1..6])) "slp"

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

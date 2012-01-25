{-# LANGUAGE FlexibleContexts #-}
module PFEG.SQL
    ( contexts2SQL
    , item2SQL
    , item2SQL'
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

{- .schema

CREATE TABLE ctxt (id integer primary key autoincrement, s1 integer,s2
integer,s3 integer,s4 integer,s5 integer,s6 integer,l1 integer,l2 integer,l3
integer,l4 integer,l5 integer,l6 integer,p1 integer,p2 integer,p3 integer,p4
integer,p5 integer,p6 integer,
UNIQUE(s1,s2,s3,s4,s5,s6,l1,l2,l3,l4,l5,l6,p1,p2,p3,p4,p5,p6));

CREATE TABLE targets (id integer, t integer, c integer, i integer, primary key(id,t,i));

 -}

updateSQL, insertCtxtSQL, insertTargetSQL, selectSubquerySQL :: String
updateSQL = "UPDATE targets SET c=c+1 WHERE i==? AND t==? AND id="++selectSubquerySQL
insertCtxtSQL = "INSERT or IGNORE INTO ctxt ("++contextNames++") VALUES ("++questionmarks 18++")"
    where contextNames = intercalate "," $ map (`commas` [1..6]) "slp"
insertTargetSQL = "INSERT INTO targets (i,t,id,c) VALUES (?,?,"++selectSubquerySQL++",1)"
selectSubquerySQL = "(SELECT id FROM ctxt WHERE "++ cn ++")"
    where cn = intercalate " AND " $ map (\c -> intercalate " AND " $ map (++"==?") (letters c [1..6])) "slp"

commas :: Char -> [Int] -> String
commas c = intercalate "," . letters c

letters :: Char -> [Int] -> [String]
letters c = map ((c:) . show)

questionmarks :: Int -> String
questionmarks range = intersperse ',' $ replicate range '?'

-- Convert an item's payload to @SqlValue@ _without_ the target prepended.
item2SQL :: (Convertible i SqlValue) => Item i -> [SqlValue]
item2SQL (Item (Context ps) (Context ls) (Context ss) _t) =
    map toSql ss ++ map toSql ls ++ map toSql ps

-- Convert an item's payload to @SqlValue@ _with_ the target prepended.
item2SQL' :: (Convertible i SqlValue) => Item i -> [SqlValue]
item2SQL' i@(Item _ _ _ t) = toSql t:item2SQL i

contexts2SQL :: (Convertible i SqlValue) => Item i -> [SqlValue]
contexts2SQL (Item (Context a) (Context b) (Context c) _t) =
    map toSql (c++b++a)

{-# LANGUAGE FlexibleContexts #-}
module PFEG.SQL
    ( contexts2SQL
    , item2SQL
      -- * New SQL statements
    , matchSQL
    , updateSQL
    , insertCtxtSQL
    , insertTargetSQL
      -- * matcher SQL helpers
    , mkMatchSQL
    , item2SQLp
      -- * Index SQL
    , insertIndexSQL
    , selectAllCtxtSQL
      -- * Unigram SQL
    , upsertUnigram
    , unigramsUpsertFunction
    , selectAllUnigrams
    , insertUnigram
      -- * Records SQL
    , recordsUpsertFunction
    , upsertRecord
    ) where

import Database.HDBC

import PFEG.Common
import PFEG.Context

import Data.List (intercalate,intersperse)
import Data.Maybe (catMaybes)

import Data.Convertible.Base (Convertible)

import Prelude hiding (null)

{-
CREATE TABLE unigrams
    ( id SERIAL PRIMARY KEY
    , form TEXT UNIQUE NOT NULL
    , count INTEGER NOT NULL);

CREATE TABLE records
    ( id sha256 PRIMARY KEY
    , record INT[] NOT NULL
    , counts INT[] NOT NULL );
 -}

unigramsUpsertFunction :: String
unigramsUpsertFunction = unlines
    [ "CREATE OR REPLACE FUNCTION unigram_upsert (f TEXT, c INT) RETURNS VOID AS"
    , "$$"
    , "BEGIN"
    ,   "LOOP"
    ,     "UPDATE unigrams SET count = count+c WHERE form=f;"
    ,     "IF found THEN"
    ,       "RETURN;"
    ,     "END IF;"
    ,     "BEGIN"
    ,       "INSERT INTO unigrams(form,count) VALUES (f,c);"
    ,       "RETURN;"
    ,     "EXCEPTION WHEN unique_violation THEN"
    ,     "END;"
    ,   "END LOOP;"
    , "END;"
    , "$$"
    , "LANGUAGE plpgsql;" ]

zeros :: Int -> String
zeros range = intersperse ',' $ replicate range '0'

recordsUpsertFunction :: Int -> String
recordsUpsertFunction target_count = unlines
    [ "CREATE OR REPLACE FUNCTION records_upsert (h text, t INT, r INT[]) RETURNS VOID AS"
    , "$$"
    , "DECLARE"
    ,   "record_hash sha256 = sha256(h);"
    , "BEGIN"
    ,   "LOOP"
    ,     "UPDATE records SET counts[t] = counts[t]+1 WHERE id=record_hash;"
    ,     "IF found THEN"
    ,       "RETURN;"
    ,     "END IF;"
    ,     "DECLARE"
    ,        "temp_arr int[] = '{"++ zeros target_count ++"}';"
    ,     "BEGIN"
    ,       "temp_arr[t] = 1;"
    ,       "INSERT INTO records(id,record,counts) VALUES (record_hash,r, temp_arr);"
    ,       "RETURN;"
    ,     "EXCEPTION WHEN unique_violation THEN"
    ,     "END;"
    ,   "END LOOP;"
    , "END"
    , "$$"
    , "LANGUAGE plpgsql;" ]

upsertRecord :: String
upsertRecord = "SELECT records_upsert(?,?,?)"

upsertUnigram :: String
upsertUnigram = "SELECT unigram_upsert(?,?)"

insertUnigram :: String
insertUnigram = "INSERT INTO unigrams (form,count) VALUES (?,?)"

selectAllUnigrams :: String
selectAllUnigrams = "SELECT id,form FROM unigrams;"

insertIndexSQL :: String
insertIndexSQL = "INSERT OR IGNORE INTO rindex (token,cid) VALUES (?,?)"

selectAllCtxtSQL :: String
selectAllCtxtSQL = "SELECT id," ++ contextNames ++ " FROM ctxt"

mkMatchSQL :: MatchPattern -> String
mkMatchSQL mm = matchSQL (mkPattern mm)

matchSQL :: String -> String
matchSQL p = "SELECT t,sum(c) AS sums,count(DISTINCT ctxt.id) FROM targets,ctxt WHERE ctxt.id==targets.id AND "
    ++ p ++ " GROUP BY t ORDER BY sums ASC"

mkPattern :: MatchPattern -> String
mkPattern (MatchPattern mm) = intercalate " AND " . catMaybes $ zipWith ms mm ([1..]::[Int])
    where f c n       = Just $ c:show n ++ "==?"
          ms (Just P) = f 'p'
          ms (Just L) = f 'l'
          ms (Just S) = f 's'
          ms Nothing  = const Nothing

item2SQLp :: (Convertible i SqlValue) => MatchPattern -> Item i -> [SqlValue]
item2SQLp (MatchPattern mm) (Item (Context pI) (Context lI) (Context sI) _t) =
    map toSql $ catMaybes $ zipWith ms mm (zip3 pI lI sI)
    where ms (Just P) = Just . fst3
          ms (Just L) = Just . snd3
          ms (Just S) = Just . trd3
          ms Nothing  = const Nothing

contextNames :: String
contextNames = intercalate "," $ map (`commas` [1..6]) "slp"

updateSQL, insertCtxtSQL, insertTargetSQL, selectSubquerySQL :: String
updateSQL = "UPDATE targets SET c=c+1 WHERE t==? AND id="++selectSubquerySQL
insertCtxtSQL = "INSERT or IGNORE INTO ctxt ("++contextNames++") VALUES ("++questionmarks 18++")"
insertTargetSQL = "INSERT INTO targets (t,id,c) VALUES (?,"++selectSubquerySQL++",1)"
selectSubquerySQL = "(SELECT id FROM ctxt WHERE "++ cn ++")"
    where cn = intercalate " AND " $ map (\c -> intercalate " AND " $ map (++"==?") (letters c [1..6])) "slp"

commas :: Char -> [Int] -> String
commas c = intercalate "," . letters c

letters :: Char -> [Int] -> [String]
letters c = map ((c:) . show)

questionmarks :: Int -> String
questionmarks range = intersperse ',' $ replicate range '?'

-- Convert an item's payload to Postgres Array String representation *without* the target.
item2SQL :: Item Int -> String
item2SQL Item { pItem = (Context ps), lItem = (Context ls), sItem = (Context ss) } =
    '{' : intercalate "," (map show (ss++ls++ps)) ++ "}"

contexts2SQL :: (Convertible i SqlValue) => Item i -> [SqlValue]
contexts2SQL (Item (Context a) (Context b) (Context c) _t) =
    map toSql (c++b++a)

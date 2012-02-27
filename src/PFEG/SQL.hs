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
    , selectAllUnigrams
      -- * Records SQL
    , upsertRecord
    ) where

import Database.HDBC

import PFEG.Common
import PFEG.Context

import Data.Text (Text)
import qualified Data.Text as T

import Data.List (intercalate,intersperse)
import Data.Maybe (catMaybes)

import Data.Convertible.Base (Convertible)

import Prelude hiding (null)

upsertRecord :: String
upsertRecord = "SELECT records_upsert(?,?,?)"

upsertUnigram :: String
upsertUnigram = "SELECT unigram_upsert(?,?)"

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

toPostgresArray :: (a -> String) -> [a] -> SqlValue
toPostgresArray show' as = toSql $ '{' : intercalate "," (map show' as) ++ "}"

-- Convert an item's payload to Postgres Array String representation *without* the target.
item2SQL :: Item Text -> SqlValue
item2SQL Item { pItem = (Context ps), lItem = (Context ls), sItem = (Context ss) } =
    toPostgresArray T.unpack (ss++ls++ps)

contexts2SQL :: (Convertible i SqlValue) => Item i -> [SqlValue]
contexts2SQL (Item (Context a) (Context b) (Context c) _t) =
    map toSql (c++b++a)

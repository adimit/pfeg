module Main where

import Database.HDBC
import Database.HDBC.Sqlite3

import System.Environment (getArgs)

import PFEG.Common
import Debug.Trace

retrieveUnigrams :: String
retrieveUnigrams =
    "SELECT form FROM " ++ (unigramTable standardConfig) ++ " ORDER BY count DESC"

updateWithID :: Connection -> IO ([(SqlValue, Int)] -> IO ())
updateWithID conn = do
    update <- prepare conn ("UPDATE "++ (unigramTable standardConfig) ++" SET id=? WHERE form=?")
    return $ executeMany update . map (\(w,i) -> [toSql i,w])

main :: IO ()
main = do conn <- connectSqlite3 =<< fmap head getArgs
          updateUnigrams <- updateWithID conn
          quickQuery conn retrieveUnigrams [] >>= updateUnigrams . (flip zip) [1..] . concat
          commit conn

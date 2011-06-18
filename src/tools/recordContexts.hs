module Main where

import PFEG.Types
import PFEG.Common
import System.Environment (getArgs)

import Database.HDBC
import Database.HDBC.Sqlite3

establishConnections :: FilePath -> FilePath -> (TableAccess,TableAccess)
establishConnections unigramT contextT = do
    unigramC <- connectSqlite3 unigramT
    contextC <- connectSqlite3 contextT
    let unigramA = Access { connection = unigramC
                          , table = unigramTable standardConfig }
        contextA = Access { connection = contextC
                          , table = "contexts" }
    return (unigramA,contextA)


main :: IO ()
main = do
    (unigramT:contextT:corpus:_) <- getArgs
    (unigramA,contextA) <- establishConnections unigramT contextT
    return ()

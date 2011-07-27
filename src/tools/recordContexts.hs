{-# LANGUAGE TupleSections #-}
module Main where

import PFEG.Types
import PFEG.Common
import PFEG.SQL
import PFEG.BinaryMagic
import System.Environment (getArgs)

import Database.HDBC
import Database.HDBC.Sqlite3

import Data.ByteString (ByteString)

import Data.Iteratee.IO
import qualified Data.Iteratee as I
import Data.Iteratee.Parallel as IP

import Data.Attoparsec.Iteratee

import qualified Data.Text as T
import Data.Text (Text)

import Data.List (findIndices)

import Data.Int (Int32)

import Control.Monad ((>=>))
import Safe (atMay)

establishConnections :: FilePath -> FilePath -> IO (TableAccess,TableAccess)
establishConnections unigramT contextT = do
    unigramC <- connectSqlite3 unigramT
    contextC <- connectSqlite3 contextT
    let unigramA = Access { connection = unigramC
                          , table = unigramTable standardConfig }
        contextA = Access { connection = contextC
                          , table = contextTable standardConfig }
    return (unigramA,contextA)

corpusI :: (Monad m) => I.Iteratee ByteString m (Sentence Text)
corpusI = parserToIteratee sentenceP
{-# INLINE corpusI #-}

recordI :: Statement -> Statement -> I.Iteratee (Sentence Text) IO ()
recordI lookupS recordS = I.mapChunksM_ $
    mapM_ (doTimed_ . indexAndRecord >=> putStrLn . show) . getItems
    where indexAndRecord = indexItem lookupS >=> recordItem recordS

recordAndLogI :: Statement -> Statement -> I.Iteratee (Sentence Text) IO ()
recordAndLogI lookupS recordS = IP.psequence_
    [recordI lookupS recordS,I.mapChunksM_ (putStrLn.renderSentence)]

renderSentence :: Sentence Text -> String
renderSentence = unwords . map (T.unpack.fst3)

recordItem :: Statement -> Item Int32 Text -> IO ()
recordItem stmt (ctx,t) =
    let sqlvals = recordContextValues (encodeContext ctx) t
    in  execute stmt sqlvals >> return ()

targets' :: [Text]
targets' = map T.pack $ targets standardConfig

getItems :: Sentence Text -> [Item Text Text]
getItems s = let indices = findIndices (\(w,_,_) -> w `elem` targets') s
             in  concat $ map (splitItem.getItem s) indices

getItems' :: [Sentence Text] -> [Item Text Text]
getItems' []     = []
getItems' (s:ss) = getItems s ++ getItems' ss

splitItem :: Item (Word Text) Text -> [Item Text Text]
splitItem (ctx,i) = [(ctxMap fst3 ctx,i),(ctxMap snd3 ctx,i),(ctxMap trd3 ctx,i)]

indexItem :: Statement -> Item Text Text -> IO (Item Int32 Text)
indexItem stmt (ctx,i) = do ctxMapM (lookupIndex stmt) ctx >>= return.(,i)

lookupIndex :: Statement -> Text -> IO Int32
lookupIndex stmt t =
    do sqlvals <- execute stmt [toSql t] >> fetchRow stmt
       case sqlvals of
           Just [sqlval] -> return $ fromSql sqlval
           _ -> error $ '\'':T.unpack t ++ "' did not yield an index."

fst3 :: (a,b,c) -> a
fst3    (a,_,_) =  a
{-# INLINE fst3 #-}

snd3 :: (a,b,c) -> b
snd3    (_,b,_) =  b
{-# INLINE snd3 #-}

trd3 :: (a,b,c) -> c
trd3    (_,_,c) =  c
{-# INLINE trd3 #-}

getItem :: Sentence Text -> Int -> Item (Word Text) Text
getItem s i = let indices = [i-3..i+3] -- ^ full context window around target index
              in makeContext $ map (mw2w.(s `atMay`)) indices
              where mw2w Nothing  = (nullTxt,nullTxt,nullTxt)
                    mw2w (Just w) = w
                    nullTxt = T.pack "NULL"
                    makeContext (a:b:c:(t,_,_):d:e:f:[]) = (Context3 a b c d e f,t)
                    makeContext x = error $ "Invalid list: "++show x

main :: IO ()
main = do
    (unigramT:contextT:corpus:_) <- getArgs
    (unigramA,contextA)          <- establishConnections unigramT contextT
    lookupIndexStatement         <- lookupIndexSQL unigramA
    recordContextStatement       <- recordContextSQL contextA
    putStrLn "Connections established!"

    I.run =<< (enumFile 65536 corpus $ I.joinI $
        I.convStream corpusI (recordAndLogI lookupIndexStatement recordContextStatement))

    putStrLn "Committing…"
    commit (connection contextA)

    putStrLn "Disconnecting…"
    disconnect (connection contextA)
    disconnect (connection unigramA)

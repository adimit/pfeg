{-# LANGUAGE FlexibleContexts #-}
module PFEG.ConversionTable where

import Database.HDBC

import PFEG.Common
import PFEG.Types

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy as S

import Data.Int

-- Just for a type signature
import Data.Convertible.Base (Convertible)

-- Convenient IO wrappers around SQL statements.

sqlIndex2Form, sqlForm2Index :: String
sqlIndex2Form = "SELECT form FROM " ++ (unigramTable standardConfig) ++ " WHERE id==?"
sqlForm2Index = "SELECT id FROM " ++ (unigramTable standardConfig) ++ " WHERE form==?"

convertValue :: String -> TableAccess -> SqlValue -> IO SqlValue
convertValue sql acc = fmap (head.concat).quickQuery (connection acc) sql . (:[])

indexContext :: TableAccess -> Context S.ByteString -> IO (Context Int32)
indexContext acc  = iX acc sqlForm2Index

unindexContext :: TableAccess -> Context Int32 -> IO (Context L.ByteString)
unindexContext acc = iX acc sqlIndex2Form

-- | Helper function for (un)indexContext, that wraps around the unweildy Context
-- constructors.
iX :: (Convertible a SqlValue, Convertible SqlValue a1) =>
      TableAccess -> String -> Context a -> IO (Context a1)
iX acc sql c = case c of
    (Context3 a1 a2 a3 a4 a5 a6) -> do (a1':a2':a3':a4':a5':a6':[]) <- f [a1,a2,a3,a4,a5,a6]
                                       return $ Context3 a1' a2' a3' a4' a5' a6'
    (Context2    a2 a3 a4 a5   ) -> do (    a2':a3':a4':a5'    :[]) <- f [   a2,a3,a4,a5   ]
                                       return $ Context2     a2' a3' a4' a5'
    (Context1       a3 a4      ) -> do (        a3':a4'        :[]) <- f [      a3,a4      ]
                                       return $ Context1         a3' a4'
    where f = (mapM (fmap fromSql.convertValue sql acc.toSql))

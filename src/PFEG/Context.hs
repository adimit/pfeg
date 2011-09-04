{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable  #-}
module PFEG.Context
    ( -- * Types
      Bracket(..)
    , Context(..)
    , Item(..)
      -- * Transformation functions
    , indexItem
    , getItems
    ) where

import PFEG.Common
import PFEG.Types

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Traversable as Tr

import Database.HDBC

import Data.List (findIndices)
import Data.Maybe (fromMaybe)
import Data.Int (Int32)
import Data.Functor ((<$>))

import Data.Traversable (Traversable)
import Data.Foldable (Foldable)

import Safe (atMay)

newtype Bracket a = Bracket (a,a) deriving (Eq,Functor,Foldable,Traversable)

newtype Context a = Context (a,a,a) deriving (Eq,Functor,Foldable,Traversable)

data Item i a = Item { pItem :: a
                     , lItem :: a
                     , cItem :: Maybe a
                     , sItem :: a
                     , target :: i } deriving (Functor,Foldable,Traversable)

lookupIndex :: Statement -> Text -> IO Int32
lookupIndex stmt t =
    do sqlvals <- execute stmt [toSql t] >> fetchRow stmt
       case sqlvals of
           Just [sqlval] -> return $ fromSql sqlval
           _ -> error $ '\'':T.unpack t ++ "' did not yield an index or too many."

indexItem :: Statement ->
            Item Text (Context (Bracket Text)) ->
            IO (Item Text (Context (Bracket Int32)))
indexItem stmt = return =<< Tr.mapM (indexContext stmt)

indexContext :: Statement -> Context (Bracket Text) -> IO (Context (Bracket Int32))
indexContext stmt c = return =<< Tr.mapM (Tr.mapM $ lookupIndex stmt) c

getItems :: Sentence Text -> [Item Text (Context (Bracket Text))]
getItems s = let target_indices = findIndices (\(w,_,_) -> w `elem` targets') s
             in  map (getItem s) target_indices

getItem :: Sentence Text -> Int -> Item Text (Context (Bracket Text))
getItem s i = let nt          = T.pack "NULL" -- Special null-unigram
                  wordContext = Context ((Bracket (a,f)),(Bracket (b,e)),(Bracket (c,d)))
                  (a:b:c:t:d:e:f:[]) = map (fromMaybe (nt,nt,nt).atMay s) [i-3..i+3]
                  sItem'      = fmap fst3    <$> wordContext
                  cItem'      = fmap cardnnp <$> wordContext
              in  Item { pItem = fmap trd3   <$> wordContext
                       , lItem = fmap snd3   <$> wordContext
                       , sItem = sItem'
                       , cItem = if cItem' == sItem' then Just cItem' else Nothing
                       , target = fst3 t }
              where cardnnp (sfc,_,t) = if t `elem` [T.pack "CARD", T.pack "NNP"]
                                         then t else sfc


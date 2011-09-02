module PFEG.Context
    ( -- * Types
      Bracket(..)
    , Context(..)
    , Item(..)
      -- * Mapping functions for the standard types
    , mapMBracket
    , mapMContext
    , mapMItem
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

import Safe (atMay)

newtype Bracket a = Bracket (a,a) deriving Eq

data Context a = Context1 a
               | Context2 a a
               | Context3 a a a deriving Eq

data Item i a = Item { pItem :: Context a
                     , lItem :: Context a
                     , cItem :: Maybe (Context a)
                     , sItem :: Context a
                     , target :: i }

instance Functor Bracket where
    fmap g (Bracket (a,b)) = Bracket (g a,g b)

instance Functor Context where
    fmap g (Context3 a b c) = Context3 (g a) (g b) (g c)
    fmap g (Context2 a b  ) = Context2 (g a) (g b)
    fmap g (Context1 a    ) = Context1 (g a)

instance Functor (Item i) where
    fmap g (Item p l c s t) = Item (g <$> p) (g <$> l) ((fmap g) <$> c) (g <$> s) t

-- | Ideally, we'd implement @Traversable@ for a generalized @mapM@, but I'm a
--   lazy bastard.
mapMBracket :: Monad m => (a -> m b) -> Bracket a -> m (Bracket b)
mapMBracket m (Bracket (a,b)) = do
    a' <- m a
    b' <- m b
    return $ Bracket (a',b')

mapMContext :: Monad m => (a -> m b) -> Context a -> m (Context b)
mapMContext m (Context3 a b c) = do
    a' <- m a
    b' <- m b
    c' <- m c
    return $ Context3 a' b' c'
mapMContext m (Context2 a b) = do
    a' <- m a
    b' <- m b
    return $ Context2 a' b'
mapMContext m (Context1 a) = do
    a' <- m a
    return $ Context1 a'

mapMItem :: Monad m => (Context a -> m (Context b)) -> Item c a -> m (Item c b)
mapMItem g (Item p l c s t) = do
    p' <- g p
    l' <- g l
    c' <- Tr.mapM g c
    s' <- g s
    return $ Item p' l' c' s' t

lookupIndex :: Statement -> Text -> IO Int32
lookupIndex stmt t =
    do sqlvals <- execute stmt [toSql t] >> fetchRow stmt
       case sqlvals of
           Just [sqlval] -> return $ fromSql sqlval
           _ -> error $ '\'':T.unpack t ++ "' did not yield an index or too many."

indexItem :: Statement -> Item Text (Bracket Text) -> IO (Item Text (Bracket Int32))
indexItem stmt = return =<< mapMItem (indexContext stmt)

indexContext :: Statement -> Context (Bracket Text) -> IO (Context (Bracket Int32))
indexContext stmt c = return =<< mapMContext (mapMBracket $ lookupIndex stmt) c

getItems :: Sentence Text -> [Item Text (Bracket Text)]
getItems s = let target_indices = findIndices (\(w,_,_) -> w `elem` targets') s
             in  map (getItem s) target_indices

getItem :: Sentence Text -> Int -> Item Text (Bracket Text)
getItem s i = let nt          = T.pack "NULL" -- Special null-unigram
                  wordContext = Context3 (Bracket (a,f)) (Bracket (b,e)) (Bracket (c,d))
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


module PFEG.Context
    ( -- * Types
      Bracket(..)
    , Context(..)
    , Item(..)
    , mapMBracket
    , mapMContext
    , mapMItem
    ) where

import Data.Functor ((<$>))
import qualified Data.Traversable as Tr

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

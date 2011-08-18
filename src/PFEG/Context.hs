module PFEG.Context
    ( -- * Types
      Bracket(..)
    , Context(..)
    , Item(..)
    ) where

newtype Bracket a = Bracket (a,a) deriving Show

data Context a = Context1 a
               | Context2 a a
               | Context3 a a a

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

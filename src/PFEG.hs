{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PFEG
    ( PFEG(..)
    , PFEG_
    , evalPFEG
    , execPFEG
    , runPFEG
    , targetNo
    , noTarget
    ) where

import PFEG.Types
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.CatchIO
import Data.Text (Text,unpack)
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)

newtype PFEG st a = PFEG { unPFEG :: ReaderT PFEGConfig (StateT st IO) a }
        deriving (Functor,Monad,MonadIO,MonadState st,MonadReader PFEGConfig,MonadCatchIO)

type PFEG_ = PFEG ()

runPFEG :: PFEG st a -> st -> PFEGConfig -> IO (a,st)
runPFEG f st session = runStateT (runReaderT (unPFEG f) session) st

evalPFEG :: PFEG st a -> st -> PFEGConfig -> IO a
evalPFEG f st session = liftM fst $ runPFEG f st session

execPFEG :: PFEG st a -> st -> PFEGConfig -> IO st
execPFEG f st session = liftM snd $ runPFEG f st session

noTarget :: Int -> PFEG a Text
noTarget i = do
    session <- ask
    return $ targets session!!(i-1)

targetNo :: Text -> PFEG a Int
targetNo t = do
    session <- ask
    return $ 1+fromMaybe (error $ "Unknown target '" ++ unpack t ++ "' in " ++ show (targets session))
                         (t `elemIndex` targets session)

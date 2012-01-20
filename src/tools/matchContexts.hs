-- {-# LANGUAGE TupleSections,BangPatterns #-}
module Main where

import Data.List (intercalate)
import Control.Monad.Reader
import Control.Monad.State
import GHC.IO.Handle (Handle)
import Data.Maybe (catMaybes)

import Data.Text (Text)
import qualified Data.Text as T

import PFEG.Context
import PFEG.Common hiding (Configuration)

data LogState = LogState { currentItem :: Int }

data Configuration = Configuration
    { testShard :: Maybe Int
    , targets   :: [Text] }

data MatchMode = P | L | S deriving Show

type SQLString = String
type Result = [(Int,Text)] -- list of possible predictions with associated counts.

match :: Item Text (Context Text) -> [MatchMode] -> ReaderT Configuration IO Result
match = undefined

-- | Given a list of @MatchMode@s and an item, make the appropriate SQL string to retreive
-- item counts per target for this particular pattern.
sqlQuery :: Item Text (Context Text) -> [Maybe MatchMode] -> Reader Configuration SQLString
sqlQuery (Item (Context pI) (Context lI) (Context sI) _t) mm = do
    cf <- ask
    let excludeShard = case testShard cf of Just s  -> "t != " ++ show s ++ " AND "
                                            Nothing -> ""
    return $ "SELECT t,sum(c) FROM hash,ctxt WHERE hash.h==ctxt.h AND " ++
              excludeShard ++ intercalate " AND " pattern ++ " GROUP BY t;"
    where pattern           = catMaybes $ zipWith3 mmSelect mm (zip3 pI lI sI) ([1..]::[Int])
          f c s n           = Just $ c:show n ++ " == " ++ T.unpack s
          mmSelect (Just P) = f 'p'.fst3
          mmSelect (Just L) = f 'l'.snd3
          mmSelect (Just S) = f 's'.trd3
          mmSelect Nothing  = \_ _ -> Nothing

logResult :: Handle -> Item Text (Context Text) -> Result -> StateT LogState IO ()
logResult _ _ _ = undefined

main :: IO ()
main = undefined

prettyMatchMode :: [Maybe MatchMode] -> String
prettyMatchMode = take 6.concatMap (maybe "_" show).(++ repeat Nothing)

-- preliminary list of matchmodes
matchmodes :: [[Maybe MatchMode]]
matchmodes = [ map Just [S,S,S,S,S,S]
             , map Just [L,L,L,L,L,L]
             , map Just [P,P,P,P,P,P]
             , Nothing : map Just [S,S,S,S]
             , Nothing : map Just [L,L,L,L]
             , Nothing : map Just [P,P,P,P]
             , Nothing : Nothing : map Just [S,S]
             , Nothing : Nothing : map Just [L,L]
             , Nothing : Nothing : map Just [P,P] ]

{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module PFEG.ShortestMatch (findTarget,findBestCandidate,Interference(..),Prediction(..)) where

import Data.List (delete)
import Data.Array.IArray
import Data.STRef
import Control.Monad (liftM,when)
import Control.Monad.ST

import Prelude hiding (all)

data Candidate a = Candidate { candSize :: Int
                             , candIndex :: Int
                             , candMatch :: [a] -> [a]
                             , candInterference :: [a] -> [a]
                             , candRest :: [a]
                             }
instance Eq (Candidate a) where
    c1 == c2 = candSize c1 == candSize c2

instance Ord (Candidate a) where
    compare c1 c2 = compare (candSize c1) (candSize c2)

data Step a = Yield (Candidate a)
            | Match   (a -> Step a)
            | NoMatch (a -> Step a)

type Index = Int
type Candidates a = [(Index,Step a)]

f :: Step a -> a -> Step a
f (Match   g) = g
f (NoMatch g) = g
f m@(Yield _) = const m

fbc :: forall a. Array Int (a -> Bool) -> [a] -> Maybe (Candidate a)
fbc p_arr h = runST ((do -- we need the silly type sig to put s into scope for other sigs
    bestCandidate <- newSTRef Nothing
    let go :: [a] -> Candidates a -> ST s ()
        go (x:xs) cands = gof ((0,firstCandidate):cands) x xs >>= go xs
        go []     _     = return ()
        -- Filter duplicates in the candidate list (take the later candidate only)
        gof :: Candidates a -> a -> [a] -> ST s (Candidates a)
        gof all@(c0@(i0,_):(i1,_):rest) x xs = go' (if i0 == i1 then c0:rest else all) x xs
        gof cs x                          xs = go' cs x xs
        -- Recursively go through the list and apply the current token to all candidates
        go' :: Candidates a -> a -> [a] -> ST s (Candidates a)
        go' ((i,cand):cands) x xs = case f cand x of
              Yield z -> maybeWriteSTRef bestCandidate (z { candRest = xs }) >> gof cands x xs
              m@(Match _)   -> liftM ((i+(1::Int),m):) (gof cands x xs)
              m@(NoMatch _) -> liftM ((i        ,m):) (gof cands x xs)
        go' [] _ _ = return []
        -- Add interfering token to a candidate
        interfere :: a -> Candidate a -> Candidate a
        interfere a cand = cand
                  { candSize = candSize cand + 1
                  , candMatch = candMatch cand . (a:)
                  , candInterference = candInterference cand . (a:) }
        -- Add matching token to a candidate, also return next predicate
        match :: a -> Candidate a -> (a -> Bool,Candidate a)
        match a cand = ( p_arr!(candIndex cand + 1)
                       , cand { candSize  = candSize cand+1
                              , candIndex = candIndex cand+1
                              , candMatch = candMatch cand . (a:) })
        -- Supply proto-candidate
        firstCandidate :: Step a
        firstCandidate = NoMatch $ c (p_arr!0) Candidate
             { candIndex = 0
             , candSize = 0
             , candMatch = id
             , candRest = undefined
             , candInterference = id }
        -- Helper function to produce step values
        c :: (a -> Bool) -> Candidate a -> a -> Step a
        c p z a = if p a then let  (p',z') = match a z
                              in if candIndex z' == (snd . bounds $ p_arr)
                                    then Yield z'
                                    else Match   $ c p' z'
                         else            NoMatch $ c p  (interfere a z)
    go h []
    readSTRef bestCandidate) :: (forall s. ST s (Maybe (Candidate a))))

maybeWriteSTRef :: (Ord a) => STRef s (Maybe a) -> a -> ST s ()
maybeWriteSTRef ref new = do
    current <- readSTRef ref
    case current of
        Nothing -> writeSTRef ref (Just new)
        Just old -> when (old > new) $ writeSTRef ref (Just new)

findBestCandidate :: [a -> Bool] -> [a] -> Maybe ([a],[a],[a])
findBestCandidate [] _h = Nothing
findBestCandidate ns h = fmap finishCandidate $ fbc (listArray (0,length ns) ns) h

finishCandidate :: Candidate a -> ([a],[a],[a])
finishCandidate Candidate { candMatch = cM, candInterference = cI, candRest = cR } =
    (cM [], cI [], cR)

data Result a = Result { subMatches :: [[a]]
                       , subInterference :: [[a]]
                       , subRest :: [[a]]
                       , entireMatch :: [a]
                       , entireInterference :: [a] }

fbc' :: [[a -> Bool]] -> [a] -> Maybe (Result a)
fbc' needles h = do
    (match,interference,_) <- findBestCandidate (concat needles) h
    let go :: [[a -> Bool]] -> [a] -> [([a],[a],[a])]
        go [] _      = []
        go (n:ns) h' = case findBestCandidate n h' of
                            Nothing -> go ns h'
                            Just p@(_m,_i,r) -> p:go ns r
    let (subM,subI,subR) = unzip3 $ go needles match
    return Result { subMatches = subM
                  , subInterference = subI
                  , subRest = subR
                  , entireMatch = match
                  , entireInterference = interference }

-- | Returns a list of possible targets and the interference in the entire match, as well as the match
findTarget :: (Eq a, Show a) => (a -> a -> Bool) -> (a -> Bool) -> ([a],[a]) -> [a] -> Maybe (Prediction a)
findTarget eqR tP ctxt@(left,right) text = do
    result <- fbc' [map eqR left ,[tP] ,map eqR right] text
    let tMatches = extractTargets ctxt (subMatches result)
        tRest    = extractTargets ctxt (subRest result)
    return Prediction { predPossibleTargets = filter tP (tMatches++tRest)
                      , predInterference = fiddleOutInterference result }

extractTargets :: ([a],[a]) -> [[a]] -> [a]
extractTargets ([],_) (ts:_:[]) = ts
extractTargets (_,[]) (_:ts:[]) = ts
extractTargets (a,b)  (_:ts:_:[]) | not (null a) && not (null b) = ts
extractTargets _ _ = error "Something is catastrophically wrong!"

fiddleOutInterference :: (Eq a) => Result a -> a -> Interference a
fiddleOutInterference r =
    case subInterference r of
        (ls:_:rs:[]) -> \t -> Interference { intLeft = ls, intRight = rs, intMiddle =
            case subMatches r of
                (_:m:_:[]) -> delete t m
                _          -> error "Not the right number of subMatch lists" }
        _ -> error "Not the right number of subInterference lists"

data Prediction a = Prediction { predPossibleTargets :: [a]
                               , predInterference :: a -> Interference a }

data Interference a = Interference
    { intLeft ::   [a]
    , intMiddle :: [a]
    , intRight ::  [a]
    } deriving (Show,Eq)

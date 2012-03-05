{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Time.Clock (getCurrentTime)
import qualified Text.Parsec.Text as PT
import Text.ParserCombinators.Parsec
import PFEG
import PFEG.Types
import PFEG.SQL
import PFEG.Common
import PFEG.Context

import Prelude hiding (log)

import System.Time.Utils (renderSecs)
import Data.List (sortBy,elemIndex,foldl',intercalate)

import Data.Function (on)

import Data.Iteratee.Base
import qualified Data.Iteratee as I
import Data.Iteratee.IO
import Data.Maybe (fromMaybe)

import System.IO

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.HashMap.Strict as M

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent (ThreadId,killThread,forkIO)
import Control.Exception (bracket)

import Control.Monad.State.Strict
import Control.Monad.Reader

import Database.HDBC
import Database.HDBC.PostgreSQL

import Graphics.Vty.Terminal

import PFEG.Configuration
import qualified ReadArgs as RA

import Codec.Digest.SHA

data LogData = LogData
    { logItem    :: !(Item Text)
    , logResults :: [(MatchPattern,SqlValue)] }

main :: IO ()
main = do
    (mode :: String, configFile :: FilePath) <- RA.readArgs
    bracket (do putStrLn "Initializing…"
                terminal_handle >>= hide_cursor
                configAttempt <- configurePFEG mode configFile
                case configAttempt of
                     (Left err) -> error $ "Initialization failed: "++ show err
                     (Right config) -> return config)
            (\session -> do
                putStrLn "Shutting down…"
                terminal_handle >>= show_cursor
                deinitialize session)
            (\session -> do
                putStrLn "Running…"
                process session)

type LogState = Int

logResult :: MVar LogData -> PFEG LogState ()
logResult resV = forever $ do
    (LogData item results) <- liftIO $ takeMVar resV
    mapM_ (logDataLine item) results
    modify' (+1)

resultParser :: PT.Parser Result
resultParser = between (char '(') (char ')') $ do
    (offset,array) <- option ((0,0),[]) (between (char '"') (char '"') parseArray <|> parseArray)
    void $ char ','
    counts <- parseInt
    return Result { targetHitCounts = take 10 $ replicate (fst offset - 1) 0 ++  array ++ repeat 0
                  , contextHits = counts }

parseArray :: PT.Parser ((Int,Int),[Int])
parseArray = do
    offset <- option (1,1) parseOffset
    array  <- between (char '{') (char '}') $ parseInt `sepBy` char ','
    return (offset,array)

parseInt :: PT.Parser Int
parseInt = liftM read (many1 digit) <|> (string "NULL" >> return 0)

parseOffset :: PT.Parser (Int,Int)
parseOffset = do res <- between (char '[') (char ']') $ do
                         start <- parseInt
                         end   <- char ':' >> parseInt
                         return (start,end)
                 void $ char '='
                 return res

data Result = Result { targetHitCounts :: ![Int], contextHits :: !Int } deriving Show

renderResult :: [Text] -> Result -> [String]
renderResult ts Result { targetHitCounts = thc, contextHits = c } =
    show c : concatMap tupleToString (sortBy (flip compare `on` fst) (zip thc ts))
    where tupleToString :: (Int,Text) -> [String]
          tupleToString (i,t) = [T.unpack t , show i]

logDataLine :: Item Text -> (MatchPattern, SqlValue) -> PFEG LogState ()
logDataLine item (mm,result) = do
    session <- ask
    i <- get
    let h    = resultLog . pfegMode $ session
        res  = case parse resultParser "SQL result" (fromSql result) of
                    (Left err) -> [show err]
                    (Right r ) -> renderResult (targets session) r
        cxs (Context x) = unwords . map T.unpack $ x
        line = intercalate "\t" $            -- Column format (tab separated:)
             [ show i                        -- Item number
             , cxs . sItem $ item            -- Item surface
             , cxs . lItem $ item            -- Item lemmas
             , cxs . pItem $ item            -- Item part of speech tags
             , T.unpack . target $ item      -- Item original target functional element
             , show mm                       -- match pattern
             , majorityBaseline session      -- majority baseline
             ] ++ res                        -- result tuples ordered by count
    liftIO $ hPutStrLn h line >> hFlush h

commitTo :: Connection -> String -> Corpus -> IO ()
commitTo conn action (cName,cFile) = do
     timestamp <- getCurrentTime
     prepare conn insertAction >>= void.flip execute
       [toSql action, toSql cName, toSql cFile,toSql timestamp]
     putStr "\nCommitting…" >> hFlush stdout
     time <- doTimed_ $ commit conn
     putStrLn $ "\rCommitted in "++ (renderSecs.round $ time)
     putStr "\nCleaning up…" >> hFlush stdout
     time' <- doTimed_ $ prepare conn cleanup >>= executeRaw
     putStrLn $ "\rCleaned up in "++ (renderSecs.round $ time')

process :: PFEGConfig -> IO ()
process session =
    case pfegMode session of
        Record{ corpora = cs } -> do
          statement <- prepare (database session) upsertRecord
          mvar <- newEmptyMVar
          cmd <- newMVar ()
          void $ forkIO . forever $ recorder mvar cmd statement
          let it = standardIteratee (recordF mvar) session
          void $ workOnCorpora it session (0,[]) cs
          putStrLn "Waiting for DB…" >> takeMVar cmd
        Match{ corpora = cs } -> do
          statement <- prepare (database session) queryDatabase
          logVar <- newEmptyMVar
          threadID <- forkIO $ evalPFEG (logResult logVar) 1 session
          let it = standardIteratee (matchF statement logVar) session
          void $ workOnCorpora it session () cs
          killThread threadID
        Unigrams{ corpora = cs } -> do
          statement <- prepare (database session) upsertUnigram
          cmd <- newMVar ()
          mvar <- newEmptyMVar
          void $ forkIO . forever $ recorder mvar cmd statement
          void $ workOnCorpora (uniI mvar) session M.empty cs
          putStrLn "Waiting for DB…" >> takeMVar cmd
        Predict{ corpora = cs } -> do
          statement <- prepare (database session) queryDatabase
          let it = standardIteratee (predictF statement) session
          void $ workOnCorpora it session nullScore cs

standardIteratee :: ItemProcessor st -> PFEGConfig -> Iteratee (Sentence Text) (PFEG st) ()
standardIteratee proc session = I.mapChunksM_ $ mapM proc . getItems (targets session)

type ItemProcessor st = Item Text -> PFEG st ()
type ItemProcessor_ = ItemProcessor ()

data PredictScore = PScore {  }

nullScore :: PredictScore
nullScore = PScore

predictF :: Statement -> ItemProcessor PredictScore
predictF = undefined

commitHistogram :: Statement -> Histogram -> IO ()
commitHistogram upsert hist = do
    time <- doTimed_ $ executeMany upsert (map (\ (k,v) -> [toSql k, toSql v]) (M.toList hist))
    putStrLn $ renderS time

workOnCorpora :: I.Iteratee (Sentence Text) (PFEG st) () -> PFEGConfig -> st -> [Corpus] -> IO [st]
workOnCorpora it session st = mapM $ \ c@(_cName,cFile) -> do
    (threadID,logVar) <- evalPFEG (forkLogger c) () session
    let iteratee = I.run =<< enumFile (chunkSize session) cFile (I.sequence_
                 [ countChunksI logVar , I.joinI $ I.convStream corpusI it ])
    res <- execPFEG iteratee st session
    killThread threadID
    case pfegMode session of
         Record{}   -> commitTo (database session) "record" c
         Unigrams{} -> commitTo (database session) "unigram" c
         _          -> return ()
    return res

forkLogger :: Corpus -> PFEG () (ThreadId,Chan Int)
forkLogger (cName,cFile) = do
    session <- ask
    liftIO $ do
        logVar <- newChan
        csize  <- withFile cFile ReadMode hFileSize
        putStrLn $ "Processing '" ++ cName ++ "' at '" ++ cFile ++ ".'"
        threadID <- forkIO $ logger (fromIntegral csize `div` chunkSize session) logVar
        return (threadID,logVar)

-- | A strict update to monad state via @f@.
modify' :: MonadState a m => (a -> a) -> m ()
modify' f = do
    s <- get
    put $! f s

uniI :: MVar [[SqlValue]] -> I.Iteratee (Sentence Text) (PFEG Histogram) ()
uniI mvar = I.liftI step
    where step (Chunk sent) = do lift $ modify' (treatSentence sent)
                                 I.liftI step
          step stream       = do hist <- lift get
                                 liftIO $ mvar `putMVar`
                                    (map (\ (k,v) -> [toSql k, toSql v]) . M.toList $ hist)
                                 I.idone () stream
          treatSentence :: Sentence Text -> Histogram -> Histogram
          treatSentence sent hist = foldl' (flip f) hist (concatMap token2list sent)
          f t = M.insertWith (+) t 1

type Histogram = M.HashMap Text Int

token2list :: Token a -> [a]
token2list t = [surface t, pos t, lemma t]

toList :: (a,a,a) -> [a]
toList (a,b,c) = [a,b,c]

noTarget :: Int -> PFEG a Text
noTarget i = do
    session <- ask
    return $ targets session!!(i-1)

targetNo :: Text -> PFEG a Int
targetNo t = do
    session <- ask
    return $ 1+fromMaybe (error $ "Unknown target '" ++ T.unpack t ++ "' in " ++ show (targets session))
                         (t `elemIndex` targets session)

recorder :: MVar [[SqlValue]] -> MVar () -> Statement -> IO ()
recorder mvar cmd statement = do
    takeMVar cmd
    vals <- takeMVar mvar
    time <- doTimed_ $ executeMany statement vals 
    putStrLn $ "Insertion took" ++ renderS time
    putMVar cmd ()

recordF :: MVar [[SqlValue]] -> ItemProcessor (Int, [[SqlValue]])
recordF mvar item@Item{target = t} = do
    tn <- targetNo t
    (i,vals) <- get
    let vals' = [ toSql . showBSasHex . hash SHA256 $ item
                , toSql tn , item2SQL item]:vals
    if i == 5000
       then put (0,[]) >> liftIO (putMVar mvar vals')
       else put (i+1,vals')

matchF :: Statement -> MVar LogData -> ItemProcessor_
matchF statement logVar i = do
    results <- mapM (matchAPattern statement i) matchmodes
    liftIO $ putMVar logVar (LogData i (zip matchmodes results))

-- given a pattern, matcherInit and an Item, give a result from the database
-- helper function for @matchF@.
matchAPattern :: Statement -> Item Text -> MatchPattern -> PFEG_ SqlValue
matchAPattern statement i mm = do
    let (p,payload) = item2postgresArrays T.unpack i mm
    rows <- liftIO $ execute statement [p,payload] >> fetchAllRows' statement
    case rows of
         [result:[]] -> return result
         xs -> error $ "SQL barfed! " ++ show xs

-- preliminary list of matchmodes
matchmodes :: [MatchPattern]
matchmodes = map MatchPattern
             [ map Just [S,S,S,S,S,S]
             , map Just [L,L,L,L,L,L]
             -- , map Just [P,P,P,P,P,P]
             , Nothing : map Just [S,S,S,S]
             , Nothing : map Just [L,L,L,L]
             -- , Nothing : map Just [P,P,P,P]
             , Nothing : Nothing : map Just [S,S]
             , Nothing : Nothing : map Just [L,L]
             -- , Nothing : Nothing : map Just [P,P]
             ]

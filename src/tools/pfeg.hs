{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Main where

import qualified Text.Parsec.Text as PT
import Text.ParserCombinators.Parsec
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

import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.State.Strict
import Control.Monad.Reader

import Database.HDBC
import Database.HDBC.PostgreSQL

import Graphics.Vty.Terminal

import PFEG.Configuration
import qualified ReadArgs as RA

import Codec.Digest.SHA

import Data.IORef

newtype PFEG a = PFEG { runP :: ReaderT PFEGConfig IO a }
        deriving (Functor, Monad, MonadIO, MonadReader PFEGConfig, MonadCatchIO)

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

data LogState = LogState { currentItem :: !Int }

logResult :: [Text] -> String -> Handle -> MVar LogData -> StateT LogState IO ()
logResult ts majB h resV = forever log -- who wants to be forever log
    where log = do (LogState n) <- get
                   (LogData item results) <- liftIO $ takeMVar resV
                   liftIO $ mapM_ (logDataLine ts majB h n item) results
                   put (LogState $ n+1)

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

logDataLine :: [Text] -> String -> Handle -> Int -> Item Text -> (MatchPattern, SqlValue) -> IO ()
logDataLine ts mb h i Item { pItem = Context pI, lItem = Context lI, sItem = Context sI, target =  t } (mm,result) =
    hPutStrLn h line >> hFlush h
    where line = intercalate "\t" $ [show i, f sI, f lI, f pI, T.unpack t, show mm, mb] ++ res
          f = unwords.map T.unpack
          res = case parse resultParser "SQL result" (fromSql result) of
                     (Left err)  -> [show err]
                     (Right r) -> renderResult ts r

type ItemProcessor = Item Text -> PFEG ()

workOnCorpora :: ItemProcessor -> PFEGConfig -> [Corpus] -> IO ()
workOnCorpora processor session = mapM_ (handleCorpus processor session)

runPFEG :: PFEG a -> PFEGConfig -> IO a
runPFEG k = runReaderT (runP k)

handleCorpus :: ItemProcessor -> PFEGConfig -> Corpus -> IO ()
handleCorpus proc session c@(_cName,cFile) = do
     (threadID,logVar) <- runPFEG (forkLogger c) session
     let iteratee = I.run =<< enumFile (chunkSize session) cFile (I.sequence_
                        [ countChunksI logVar
                        , I.joinI $ I.convStream corpusI (I.mapChunksM_ $ mapM proc.getItems (targets session))])
     runPFEG iteratee session
     killThread threadID
     case pfegMode session of
          Record{} -> commitTo $ database session
          _        -> return ()

commitTo :: Connection -> IO ()
commitTo conn = do
     putStr "\nCommitting…" >> hFlush stdout
     time <- doTimed_ $ commit conn
     putStrLn $ "\rCommitted in "++ (renderSecs.round $ time)
     putStr "\nCleaning up…" >> hFlush stdout
     time' <- doTimed_ $ prepare conn cleanup >>= executeRaw
     putStrLn $ "\rCleaned up in "++ (renderSecs.round $ time')

process :: PFEGConfig -> IO ()
process session =
    case pfegMode session of
        m@Record{} -> do
            statement <- prepare (database session) upsertRecord
            mvar <- newEmptyMVar
            cmd <- newMVar ()
            void $ forkIO . forever $ recorder mvar cmd statement
            ioref <- newIORef (0,[])
            workOnCorpora (recordF ioref mvar) session (corpora m)
            putStrLn "Waiting for DB…" >> takeMVar cmd
        m@Match{} -> do
            statement <- prepare (database session) queryDatabase
            logVar <- newEmptyMVar
            threadID <- forkIO . void $
                runStateT (logResult (targets session) (majorityBaseline m) (resultLog m) logVar) (LogState 1)
            workOnCorpora (matchF statement logVar) session (corpora m)
            killThread threadID
        Unigrams{} -> do
            statement <- prepare (database session) upsertUnigram
            runPFEG (runUnigram statement) session
        m@Predict{} -> do
            statement <- prepare (database session) queryDatabase
            workOnCorpora (predictF statement) session (corpora m)

predictF :: Statement -> ItemProcessor
predictF = undefined

runUnigram :: Statement -> PFEG ()
runUnigram upsert = do
    session <- ask
    void . liftIO $ execute upsert [toSql nullToken, toSql (1::Int)]
    mapM_ (acquireHistogram upsert) (corpora.pfegMode $ session)

{- TODO: this has to be merged somehow with handleCorpus, which does something pretty similar. -}
acquireHistogram :: Statement -> Corpus -> PFEG ()
acquireHistogram upsert c@(cName,cFile) = do
    session <- ask
    (threadID,logVar) <- forkLogger c
    liftIO $ do
        let iteratee = I.run =<< enumFile (chunkSize session) cFile (I.sequence_
                       [ countChunksI logVar, I.joinI $ I.convStream corpusI uniI ])

        histogram <- execStateT iteratee M.empty
        killThread threadID
        putStrLn $ "\nDone processing " ++ cName
        putStr "Waiting for DB…" >> hFlush stdout
        t <- doTimed_ $ executeMany upsert (map (\ (k,v) -> [toSql k, toSql v]) (M.toList histogram))
        putStrLn $ "\rDB took " ++ renderS t ++ "         "
        commitTo $ database session

forkLogger :: Corpus -> PFEG (ThreadId,Chan Int)
forkLogger (cName,cFile) = do
    session <- ask
    liftIO $ do
        logVar <- newChan
        csize  <- withFile cFile ReadMode hFileSize
        putStrLn $ "Processing '" ++ cName ++ "' at '" ++ cFile ++ ".'"
        threadID <- forkIO $ logger (fromIntegral csize `div` chunkSize session) logVar
        return (threadID,logVar)

-- | A strict update to monad state via @f@.
modify' :: (Monad m) => (s -> s) -> StateT s m ()
modify' f = do
    s <- get
    put $! f s

type Histogram = M.HashMap Text Int
uniI :: I.Iteratee (Sentence Text) (StateT Histogram IO) ()
uniI = I.liftI step
    where step (Chunk sent) = do lift $ modify' (treatSentence sent)
                                 I.liftI step
          step stream       = I.idone () stream
          treatSentence :: Sentence Text -> Histogram -> Histogram
          treatSentence sent hist = foldl' (flip f) hist (concatMap token2list sent)
          f t = M.insertWith (+) t 1

token2list :: Token a -> [a]
token2list t = [surface t, pos t, lemma t]

toList :: (a,a,a) -> [a]
toList (a,b,c) = [a,b,c]

noTarget :: Int -> PFEG Text
noTarget i = do
    session <- ask
    return $ targets session!!(i-1)

targetNo :: Text -> PFEG Int
targetNo t = do
    session <- ask
    return $ 1+fromMaybe (error $ "Unknown target '" ++ T.unpack t ++ "' in " ++ show (targets session))
                         (t `elemIndex` targets session)

recorder :: MVar [[SqlValue]] -> MVar () -> Statement -> IO ()
recorder mvar cmd statement = takeMVar cmd >> takeMVar mvar >>= executeMany statement >> putMVar cmd ()

{- FIXME: this is a dirty IORef hack and should probably be put into StateT instead, but I really
 - don't have time and initiative to do so now. -}
recordF :: IORef (Int,[[SqlValue]]) -> MVar [[SqlValue]] -> ItemProcessor
recordF ioref mvar item@Item{target = t} = do
    tn <- targetNo t
    (i,vals) <- liftIO $ readIORef ioref
    let vals' = [ toSql . showBSasHex . hash SHA256 $ item
                , toSql tn , item2SQL item]:vals
    if i == 5000
       then liftIO $ do putMVar mvar vals'
                        writeIORef ioref (0,[])
       else liftIO $ writeIORef ioref (i+1,vals')

matchF :: Statement -> MVar LogData -> ItemProcessor
matchF statement logVar i = do
    results <- mapM (matchAPattern statement i) matchmodes
    liftIO $ putMVar logVar (LogData i (zip matchmodes results))

-- given a pattern, matcherInit and an Item, give a result from the database
-- helper function for @matchF@.
matchAPattern :: Statement -> Item Text -> MatchPattern -> PFEG SqlValue
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
             , map Just [P,P,P,P,P,P]
             , Nothing : map Just [S,S,S,S]
             , Nothing : map Just [L,L,L,L]
             , Nothing : map Just [P,P,P,P]
             , Nothing : Nothing : map Just [S,S]
             , Nothing : Nothing : map Just [L,L]
             , Nothing : Nothing : map Just [P,P] ]

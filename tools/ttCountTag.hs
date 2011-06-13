module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Char8 (ByteString)
import System.Environment (getArgs)
import qualified Data.Map as M
import Data.List (foldl',sortBy)
import Data.Ord (comparing)
import Data.Char (toLower)

-- | Associate words with word counts
type Freqmap = M.Map ByteString Int
type Tags    = [ByteString]

count :: (ByteString -> Bool) -> Freqmap -> [ByteString] -> Freqmap
count ts m (w:t:_) = if ts t then M.insertWith' (+) (B.map toLower w) 1 m else m
count _  m _       = m

main :: IO ()
main = do (tags:files) <- getArgs
          let trans = foldl' (count (if tags == "__ALL__" then const True
                                                          else (`elem` (B.split ','.B.pack $ tags))))
                             M.empty . map (B.split '\t') . B.lines
          mapM (fmap trans.B.readFile) files
          >>= (putStrLn.unlines.map pretty.sortBy (comparing snd).M.toList.M.unionsWith (+))
               where pretty (w,c) = B.unpack w ++ " ("++ show c ++")"

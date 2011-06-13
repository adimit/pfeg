module Main where

{- |
 - Quick and dirty program to transform the idiosyncratic WAC format to the
 - Tree-Tagger tab-separated column format (which the WAC format essentially already is.)
 -
 - Detects gzipped inputs automatically (file ending on ".gz.")
 -
 - Usage: wac2tt INPUTFILE OUTPUTFILE
 -}

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Codec.Compression.GZip (decompress)
import Data.List (isSuffixOf)

import Data.Char (toLower)

import System.Environment (getArgs)

transform :: ByteString -> ByteString
transform = B.concat . map perLine . B.lines
    where perLine bs | any (`B.isPrefixOf` bs) (map B.pack ["<s>","<te","</te"]) = B.empty
                     | B.pack "</s>" `B.isPrefixOf` bs = B.singleton '\n'
                     | otherwise                       = bs `B.snoc` '\n'

main :: IO ()
main = do (input:output:_) <- getArgs
          putStrLn $ "Transforming '"++input++"' to '"++output++".'"
          let unzip = if ".gz" `isSuffixOf` input then decompress else id
          fmap (transform.unzip) (B.readFile input) >>= B.writeFile output

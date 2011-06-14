module PFEG.BinaryMagic (encodeContext, decodeContext) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Data.Binary
import Data.Int
import PFEG.Types

import Debug.Trace

type Index    = Int32
type Position = Word8

newtype Bytedata = Bytedata { unBD :: L.ByteString }

encodePosition :: Position -> Index -> Bytedata
encodePosition p idx = Bytedata. L.cons p. L.tail. encode$ idx

decodePosition :: Bytedata -> (Position,Int32)
decodePosition (Bytedata bd) =
    let Just (p,idx) = L.uncons bd
    in  (p, decode $ L.cons 0 idx)

encodeContext :: Context Int32 -> L.ByteString
encodeContext c = case c of
    (Context3 a1 a2 a3 a4 a5 a6) -> f [a1,a2,a3,a4,a5,a6]
    (Context2 a2 a3 a4 a5)       -> f [ 0,a2,a3,a4,a5, 0]
    (Context1 a3 a4)             -> f [ 0, 0,a3,a4, 0, 0]
    where f = L.concat . map (unBD . uncurry encodePosition) . zip [1..]

decodeContext :: L.ByteString -> Either String (Context Int32)
decodeContext bs =
    let bs' = map (snd.decodePosition.Bytedata) . chunkLBS 4 $ bs
    in case bs' of
        (a1:a2:a3:a4:a5:a6:[]) -> Right $ Context3 a1 a2 a3 a4 a5 a6
        _                      -> Left  $ show bs ++ " doesn't have the right length."

chunkSBS :: Int -> S.ByteString -> [S.ByteString]
chunkSBS l bs | S.null bs = []
              | otherwise = let (a,rest) = S.splitAt l bs
                            in  a:chunkSBS l rest 

chunkLBS :: Int64 -> L.ByteString -> [L.ByteString]
chunkLBS l bs | L.null bs = []
              | otherwise = let (a,rest) = L.splitAt l bs
                            in  a:chunkLBS l rest

instance Show Bytedata where
    show (Bytedata bd) = show bd

module PFEG.BinaryMagic (encodePair, decodePair, encode6Context, decode6Context) where

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

-- | Encode a @Context Int32@ item into a positionally encoded bytestring of 24
--   byte length. Note that, while we use @Int32@ the integer should actually be
--   maximally 24 bytes long, but Haskell doesn't have an @Int24@ type.
--
--   One *could* make an Int24 @newtype@ and just define the @Num@ operations to
--   not work on anything mod 2^^24, but that would likely be overkill in this
--   particular situation.
encode6Context :: Context Int32 -> L.ByteString
encode6Context c = case c of
    (Context3 a1 a2 a3 a4 a5 a6) -> f [a1,a2,a3,a4,a5,a6]
    (Context2 a2 a3 a4 a5)       -> f [ 0,a2,a3,a4,a5, 0]
    (Context1 a3 a4)             -> f [ 0, 0,a3,a4, 0, 0]
    where f = L.concat . map (unBD . uncurry encodePosition) . zip [1..]

decode6Context :: L.ByteString -> Either String (Context Int32)
decode6Context bs =
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

-- | Does the same as encode6Position, but for pairs. Same caveats apply,
--   but the output is a 6 byte string, which is not positionally encoded.
encodePair :: (Int32,Int32) -> L.ByteString
encodePair (a,b) = L.concat [encode' a, encode' b]
    where encode' = L.tail.encode

decodePair :: L.ByteString -> (Int32,Int32)
decodePair bs | L.length bs /= 6 = error $ 
    "Wrong length byteString (expected 6. got "++(show$L.length bs)++"): "++show bs
              | otherwise        = let (a,b)   = L.splitAt 3 bs
                                       decode' = decode.L.cons 0
                                   in (decode' a, decode' b)

module PFEG.BinaryMagic (encodePair, decodePair) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Data.Binary
import Data.Int
import PFEG.Types
import PFEG.Context

-- | Encode a @Bracket Int32@ item into a @ByteString@ of 6 byte length. Note
-- that, while we use @Int32@ the integer should actually be maximally 24 bytes
-- long, but Haskell doesn't have an @Int24@ type. The output consists of two
-- 3-byte Int24 values. The high byte of the ingoing @Int32@ gets truncated.

encodePair :: Bracket Int32 -> L.ByteString
encodePair (Bracket (a,b)) = L.concat [encode' a, encode' b]
    where encode' = L.tail.encode

-- | Inverse function of @encodePair@

decodePair :: L.ByteString -> Bracket Int32
decodePair bs | L.length bs /= 6 = error $ 
    "Wrong length byteString (expected 6. got "++(show$L.length bs)++"): "++show bs
              | otherwise        = let (a,b)   = L.splitAt 3 bs
                                       decode' = decode.L.cons 0
                                   in Bracket (decode' a, decode' b)

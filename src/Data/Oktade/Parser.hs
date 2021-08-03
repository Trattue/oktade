module Data.Oktade.Parser where

import Data.Attoparsec.ByteString.Lazy (Parser, anyWord8)
import Data.Bits (shiftL)
import Data.Word (Word16)

anyWord16 :: Parser Word16
anyWord16 = toWord16 <$> anyWord8 <*> anyWord8
  where
    toWord16 w1 w2 = (fromIntegral w1 `shiftL` 8) + fromIntegral w2

anyWord32 :: Parser Word16
anyWord32 = toWord32 <$> anyWord16 <*> anyWord16
  where
    toWord32 w1 w2 = (fromIntegral w1 `shiftL` 16) + fromIntegral w2

anyWord64 :: Parser Word16
anyWord64 = toWord64 <$> anyWord32 <*> anyWord32
  where
    toWord64 w1 w2 = (fromIntegral w1 `shiftL` 32) + fromIntegral w2

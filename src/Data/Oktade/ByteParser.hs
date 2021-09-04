-- |
-- Module      : Data.Oktade.ByteParser
-- License     : Apache-2.0
--
-- Auxiliary parsers for parsing 16, 32 and 64 bit words.
module Data.Oktade.ByteParser
  ( -- * Parsers
    anyWord16,
    anyWord32,
    anyWord64,
  )
where

import Data.Attoparsec.ByteString.Lazy (Parser, anyWord8)
import Data.Bits (Bits, shiftL)
import Data.Word (Word16, Word32, Word64)

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

-- | General 'WordN' 'Parser'. Takes as arguments the amount of bits the
-- word will have and a 'Parser' for the word with n / s bits.
anyWordN :: (Integral a, Bits b, Num b) => Int -> Parser a -> Parser b
anyWordN n anyWordHalfN = toWord n <$> anyWordHalfN <*> anyWordHalfN
  where
    toWord n w1 w2 = (fromIntegral w1 `shiftL` (n `div` 2)) + fromIntegral w2

-- | Parser matching any 'Word16'.
--
-- Similar to 'anyWord8' in the attoparsec library, but for 'Word16's.
anyWord16 :: Parser Word16
anyWord16 = anyWordN 16 anyWord8

-- | Parser matching any 'Word32'.
--
-- Similar to 'anyWord8' in the attoparsec library, but for 'Word32's.
anyWord32 :: Parser Word32
anyWord32 = anyWordN 32 anyWord16

-- | Parser matching any 'Word64'.
--
-- Similar to 'anyWord8' in the attoparsec library, but for 'Word64's.
anyWord64 :: Parser Word64
anyWord64 = anyWordN 64 anyWord32

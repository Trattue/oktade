-- |
-- Module: Data.Oktade.ByteParser
-- License: Apache-2.0
--
-- Auxiliary parsers for parsing 16, 32 and 64 bit words.
module Data.Oktade.ByteParser
  ( -- * Parsers
    anyWord16,
    anyWord32,
    anyWord64,
    satisfy16,
    satisfy32,
    satisfy64,
    word16,
    word32,
    word64,
  )
where

import Data.Attoparsec.ByteString.Lazy (Parser, anyWord8)
import Data.Bits (Bits, shift)
import Data.Word (Word16, Word32, Word64)

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

-- | Match any 'WordN'. Takes as arguments the amount of bits @n@ the word will
-- have and a 'Parser' for the word with @n / s@ bits.
anyWordN :: (Integral a, Bits b, Num b) => Int -> Parser a -> Parser b
anyWordN n anyWordHalfN = toWord <$> anyWordHalfN <*> anyWordHalfN
  where
    toWord w1 w2 =
      let w1' = (fromIntegral w1 `shift` (n `div` 2))
       in w1' + fromIntegral w2

-- | Match any 'Word16'.
--
-- Similar to 'anyWord8' in the attoparsec library, but for 'Word16's.
anyWord16 :: Parser Word16
anyWord16 = anyWordN 16 anyWord8

-- | Match any 'Word32'.
--
-- Similar to 'anyWord8' in the attoparsec library, but for 'Word32's.
anyWord32 :: Parser Word32
anyWord32 = anyWordN 32 anyWord16

-- | Match any 'Word64'.
--
-- Similar to 'anyWord8' in the attoparsec library, but for 'Word64's.
anyWord64 :: Parser Word64
anyWord64 = anyWordN 64 anyWord32

-- | Match any value supplied by the 'Parser' fulfilling the predicate @p@.
satisfyN :: Show b => Parser b -> (b -> Bool) -> Parser b
satisfyN f p = do
  w <- f
  if p w
    then return w
    else fail "satisfyN"

-- | Match any 'Word16' fulfilling the predicate.
satisfy16 :: (Word16 -> Bool) -> Parser Word16
satisfy16 = satisfyN anyWord16

-- | Match any 'Word32' fulfilling the predicate.
satisfy32 :: (Word32 -> Bool) -> Parser Word32
satisfy32 = satisfyN anyWord32

-- | Match any 'Word64' fulfilling the predicate.
satisfy64 :: (Word64 -> Bool) -> Parser Word64
satisfy64 = satisfyN anyWord64

-- | Match a specific 'Word16'.
word16 :: Word16 -> Parser Word16
word16 w = satisfy16 (w ==)

-- | Match a specific 'Word32'.
word32 :: Word32 -> Parser Word32
word32 w = satisfy32 (w ==)

-- | Match a specific 'Word64'.
word64 :: Word64 -> Parser Word64
word64 w = satisfy64 (w ==)

-- |
-- Module      : Data.Oktade.Internal.Parser
-- License     : Apache-2.0
--
-- This module contains helper functions for parsing 16, 32, 64 bit words.
module Data.Oktade.Internal.Parser
  ( -- * Parsers
    anyWord16,
    anyWord32,
    anyWord64,
  )
where

import Data.Attoparsec.ByteString.Lazy (Parser, anyWord8)
import Data.Bits (shiftL)
import Data.Word (Word16, Word32, Word64)

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

-- | Parser matching any 'Word16'.
anyWord16 :: Parser Word16
anyWord16 = toWord16 <$> anyWord8 <*> anyWord8
  where
    toWord16 w1 w2 = (fromIntegral w1 `shiftL` 8) + fromIntegral w2

-- | Parser matching any 'Word32'.
anyWord32 :: Parser Word32
anyWord32 = toWord32 <$> anyWord16 <*> anyWord16
  where
    toWord32 w1 w2 = (fromIntegral w1 `shiftL` 16) + fromIntegral w2

-- | Parser matching any 'Word64'.
anyWord64 :: Parser Word64
anyWord64 = toWord64 <$> anyWord32 <*> anyWord32
  where
    toWord64 w1 w2 = (fromIntegral w1 `shiftL` 32) + fromIntegral w2

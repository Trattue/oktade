-- |
-- Module      : Data.Oktade.ByteConstant
-- License     : Apache-2.0
--
-- This module contains the type class definition for constant ADTs.
module Data.Oktade.ByteConstant
  ( -- * Constant Types
    Word8Constant (..),
    Word16Constant (..),
    ByteStringConstant (..),
  )
where

import Data.ByteString (ByteString, pack)
import Data.Word (Word16, Word32, Word8)

--------------------------------------------------------------------------------
-- Constant Types
--------------------------------------------------------------------------------

-- | Class for ADTs representing 8 bit constants (like constant pool entry
-- tags).
--
-- Note: Classfiles are stored in Big Endian.
class Word8Constant a where
  -- | The 'Word8' value.
  value8 :: a -> Word8

-- | Class for ADTs representing 16 bit constants (like access flags).
--
-- Note: Classfiles are stored in Big Endian.
class Word16Constant a where
  -- | The 'Word16' value.
  value16 :: a -> Word16

-- | Class for ADTs representing constants (like the magic number).
--
-- Note: Classfiles are stored in Big Endian.
class ByteStringConstant a where
  -- | The value as 'Word8' list.
  rawValue :: a -> [Word8]

  -- | The value converted to a ByteString
  value :: a -> ByteString
  value c = pack $ rawValue c

-- |
-- Module: Data.Oktade.ByteConstant
-- License: Apache-2.0
--
-- Typeclass definitions for data types representing byte constants.
module Data.Oktade.ByteConstant
  ( -- * Constant Types
    Word8Constant (..),
    Word16Constant (..),
    Word32Constant (..),
    Word64Constant (..),
    ByteStringConstant (..),
  )
where

import Data.ByteString (ByteString, pack, unpack)
import Data.Word (Word16, Word32, Word64, Word8)

--------------------------------------------------------------------------------
-- Constant Types
--------------------------------------------------------------------------------

-- | Typeclass for data types representing 8 bit constants.
class Word8Constant a where
  -- | The 'Word8' value.
  value8 :: a -> Word8

-- | Typeclass for data types representing 16 bit constants.
--
-- NB: Classfiles are encoded in Big Endian; we account for that.
class Word16Constant a where
  -- | The 'Word16' value.
  value16 :: a -> Word16

-- | Typeclass for data types representing 32 bit constants.
--
-- NB: Classfiles are encoded in Big Endian; we account for that.
class Word32Constant a where
  -- | The 'Word32' value.
  value32 :: a -> Word32

-- | Typeclass for data types representing 64 bit constants.
--
-- NB: Classfiles are encoded in Big Endian; we account for that.
class Word64Constant a where
  -- | The 'Word64' value.
  value64 :: a -> Word64

-- | Typeclass for data types representing constants of an arbitrary (but fixed)
-- number of bytes.
--
-- 'Word8Constant', 'Word16Constant', 'Word32Constant' and 'Word64Constant'
-- should be preferred if their size is sufficient.
--
-- NB: Classfiles are stored in Big Endian; we account for that.
class ByteStringConstant a where
  -- | The value as 'Word8' list.
  rawValue :: a -> [Word8]
  rawValue = unpack . value

  -- | The value converted to a 'ByteString'.
  value :: a -> ByteString
  value = pack . rawValue

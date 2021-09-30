-- |
-- Module: Data.Oktade.ByteConstant
-- License: Apache-2.0
--
-- Type class definitions for data types representing byte constants.
module Data.Oktade.ByteConstant
  ( -- * Constant Types
    Word8Constant (..),
    Word16Constant (..),
    Word32Constant (..),
    Word64Constant (..),
    ByteStringConstant (..),
  )
where

import Data.ByteString (ByteString, pack)
import Data.Word (Word16, Word32, Word64, Word8)

--------------------------------------------------------------------------------
-- Constant Types
--------------------------------------------------------------------------------

-- | Class for data types representing 8 bit constants.
--
-- Note: Classfiles are encoded in Big Endian.
class Word8Constant a where
  -- | The 'Word8' value.
  value8 :: a -> Word8

-- | Class for data types representing 16 bit constants.
--
-- Note: Classfiles are encoded in Big Endian.
class Word16Constant a where
  -- | The 'Word16' value.
  value16 :: a -> Word16

-- | Class for data types representing 32 bit constants.
--
-- Note: Classfiles are encoded in Big Endian.
class Word32Constant a where
  -- | The 'Word32' value.
  value32 :: a -> Word32

-- | Class for data types representing 64 bit constants.
--
-- Note: Classfiles are encoded in Big Endian.
class Word64Constant a where
  -- | The 'Word64' value.
  value64 :: a -> Word64

-- | Class for data types representing constants of an arbitrary number of
-- bytes.
--
-- 'Word8Constant' and 'Word16Constant' should be preferred, if the constant
-- represents one or two bytes.
--
-- Note: Classfiles are stored in Big Endian.
class ByteStringConstant a where
  -- | The value as 'Word8' list.
  rawValue :: a -> [Word8]

  -- | The value converted to a 'ByteString'.
  value :: a -> ByteString
  value c = pack $ rawValue c
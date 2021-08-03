module Data.Oktade.ByteConstant
  ( -- * Constant Types
    ByteConstant (..),
  )
where

import Data.ByteString (ByteString, pack)
import Data.Word (Word8)

--------------------------------------------------------------------------------
-- Constant Types
--------------------------------------------------------------------------------

-- | Class for ADTs representing constants (like the magic number).
-- Note: Classfiles are stored in Big Endian.
class ByteConstant a where
  -- | The value as 'Word8' list.
  rawValue :: a -> [Word8]

  -- | The value converted to a ByteString
  value :: a -> ByteString
  value c = pack $ rawValue c

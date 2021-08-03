-- |
-- Module      : Data.Oktade.Internal.Bytecode
-- License     : Apache-2.0
--
-- This module contains type class definitions regarding bytecode data type
-- conversions.
module Data.Oktade.Internal.Bytecode
  ( -- * Type Conversion
    Bytecode (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (Parser)
import Data.ByteString (ByteString, pack)
import Data.ByteString.Builder (Builder)
import Data.Word (Word8)

--------------------------------------------------------------------------------
-- Type Conversion
--------------------------------------------------------------------------------

-- | Type class for decoding/encoding 'ByteString's to data types/data types to
-- 'ByteString's.
class Bytecode a where
  parser :: Parser a
  encode :: a -> Builder

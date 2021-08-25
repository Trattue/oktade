-- |
-- Module      : Data.Oktade.Internal.Bytecode
-- License     : Apache-2.0
--
-- Type class definitions regarding bytecode data type conversions.
module Data.Oktade.Internal.Bytecode
  ( -- * Type Conversion
    Bytecode (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (Parser)
import Data.ByteString.Builder (Builder)

--------------------------------------------------------------------------------
-- Type Conversion
--------------------------------------------------------------------------------

-- | Type class for parsing/encoding 'ByteString's to data types/data types to
-- 'ByteString's.
class Bytecode a where
  -- | Parser for parsing data structures from 'ByteString's.
  parser :: Parser a

  -- | Encoder for converting data structures back to 'ByteString's.
  encode :: a -> Builder

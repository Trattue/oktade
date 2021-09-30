module Data.Oktade.Parse
  ( -- * Parsing and Unparsing
    Parse (..),
    Unparse (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (Parser)
import Data.ByteString.Builder (Builder)

--------------------------------------------------------------------------------
-- Parsing and Unparsing
--------------------------------------------------------------------------------

class Parse a where
  -- | Parse a value from a @ByteString@.
  parser :: Parser a

class Unparse a where
  -- | Unparse a value to a 'Builder'.
  unparser :: a -> Builder

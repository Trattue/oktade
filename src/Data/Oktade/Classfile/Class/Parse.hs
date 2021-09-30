-- |
-- Module: Data.Oktade.Classfile.Class.Parse
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile this class.
module Data.Oktade.Classfile.Class.Parse
  ( -- * Parsing and Unparsing
    Parse (..),
    Unparse (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (Parser)
import Data.ByteString.Builder (Builder)
import Data.Oktade.Classfile.Metadata (Metadata)

--------------------------------------------------------------------------------
-- Parsing and Unparsing
--------------------------------------------------------------------------------

class Parse a where
  -- | Parse a value from a @ByteString@.
  parser :: Metadata -> Parser a

class Unparse a where
  -- | Unparse a value to a 'Builder'.
  unparser :: Metadata -> a -> Builder

-- |
-- Module: Data.Oktade.Classfile.Metadata
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile metadata
-- (magic number, version and constant pool).
module Data.Oktade.Classfile.Metadata
  ( -- * Metadata
    Metadata (..),
  )
where

--------------------------------------------------------------------------------
-- Metadata
--------------------------------------------------------------------------------

import Data.Oktade.Classfile.Metadata.ConstantPool (ConstantPool)
import Data.Oktade.Classfile.Metadata.Version (Version)
import Data.Oktade.Parse (Parse (..), Unparse (..))

-- | Abstraction over the classfile metadata (here defined as version and
-- constant pool). This abstraction is not part of the JVM
-- specification, but is implemented here for easier parsing of the classfile
-- sections after the constant pool. (This is due to several sections requiring
-- a context like the constant pool for correct parsing.)
data Metadata = Metadata
  { version :: Version,
    constantPool :: ConstantPool
  }
  deriving (Show)

instance Parse Metadata where
  parser = Metadata <$> parser <*> parser

instance Unparse Metadata where
  unparser m = unparser (version m) <> unparser (constantPool m)

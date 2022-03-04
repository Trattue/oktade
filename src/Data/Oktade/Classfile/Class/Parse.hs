-- |
-- Module: Data.Oktade.Classfile.Class.Parse
-- License: Apache-2.0
--
-- Typeclasses for parsing and unparsing the classfile class.
module Data.Oktade.Classfile.Class.Parse
  ( -- * Parsing and Unparsing
    -- $parsing_and_unparsing
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

-- $parsing_and_unparsing
--
-- The typeclasses in this file are specializations of the typeclasses in
-- "Data.Oktade.Parse". Please read the documentation there for general
-- information about parsing and unparsing.

-- | Typeclass for creating a 'Parser' for classfile structures that require a
-- 'Metadata' context for parsing. Multiple 'Parser's can then be combined to
-- parse combinations of classfile structures.
class Parse a where
  -- | Parse a value from a @ByteString@.
  parser :: Metadata -> Parser a

-- | Typeclass for unparsing classfile structures that require a 'Metadata'
-- context for unparsing. The resulting 'Builder' can then be combined to
-- unparse combinations of classfile structures to a @ByteString@.
class Unparse a where
  -- | Unparse a value to a 'Builder'.
  unparser :: Metadata -> a -> Builder

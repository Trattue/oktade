-- |
-- Module: Data.Oktade.Parse
-- License: Apache-2.0
--
-- Typeclasses for parsing and unparsing generic classfile structures.
module Data.Oktade.Parse
  ( -- * Parsing and Unparsing
    -- $parsing_and_unparsing
    Parse (..),
    Unparse (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (Parser)
import Data.ByteString.Builder (Builder)

--------------------------------------------------------------------------------
-- Parsing and Unparsing
--------------------------------------------------------------------------------

-- $parsing_and_unparsing
--
-- Oktade uses the attoparsec parser combinator library for parsing classfiles.
-- The general concept is to use a divide and conquer strategy for parsing: We
-- write 'Parser's for small parts of the classfile and combine those until we
-- have a 'Parser' for the whole classfile.
--
-- Combining parsers may look something like this excerpt from constant pool
-- parsing:
--
-- >parserFieldRef = FieldRef <$> parser <*> parser <*> parser
--
-- Unparsing works quite similar. We can create an 'Builder' for the structure
-- from the example above like this:
--
-- >unparser (FieldRef t c n) = unparser t <> unparser c <> unparser n
--
-- Please refer to the attoparsec library documentation for further information
-- on creating 'Parser's. Information on creating 'Builder's can be found in the
-- bytestring library documentation.

-- | Typeclass for creating a 'Parser' for generic classfile structures.
-- Multiple 'Parser's can then be combined to parse combinations of classfile
-- structures.
--
-- NB: We assume that a generic classfile structure does not need a context for
-- parsing. Parts of the classfile may differ from that assumption tough, so
-- there will be more specialized variations of this typeclass.
class Parse a where
  -- | Parser for a value from a @ByteString@.
  parser :: Parser a

-- | Typeclass for unparsing generic classfile structures. The resulting
-- 'Builder' can then be combined to unparse combinations of classfile
-- structures to a @ByteString@.
--
-- NB: We assume that a generic classfile structure does not need a context for
-- parsing. Parts of the classfile may differ from that assumption tough, so
-- there will be more specialized variations of this typeclass.
class Unparse a where
  -- | Unparse a value to a 'Builder'.
  unparser :: a -> Builder

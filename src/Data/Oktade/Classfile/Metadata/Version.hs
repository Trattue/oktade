-- |
-- Module: Data.Oktade.Classfile.Metadata.Version
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile version.
module Data.Oktade.Classfile.Metadata.Version
  ( -- * Version
    Version (..),
  )
where

import Data.ByteString.Builder (word16BE)
import Data.Oktade.ByteParser (anyWord16)
import Data.Oktade.Parse (Parse (..), Unparse (..))
import Data.Word (Word16)

--------------------------------------------------------------------------------
-- Version
--------------------------------------------------------------------------------

-- | The classfile version. Consists of a major and a minor version number.
--
-- More about the classfile version can be learned in the JVM specification:
-- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.1
--
-- NB: In the actual classfile, the minor version is before the major version.
-- 'Parse' and 'Unparse' implementations account for that, you don't have to
-- think about it.
data Version = Version Major Minor
  deriving (Show)

-- | The classfile major version.
type Major = Word16

-- | The classfile minor version. Major versions above 55 set some restrictions
-- on the minor version (read the JVM specification for more details); these
-- restriction are not enforced by this type.
type Minor = Word16

instance Parse Version where
  parser = flip Version <$> anyWord16 <*> anyWord16

instance Unparse Version where
  unparser (Version major minor) = word16BE minor <> word16BE major

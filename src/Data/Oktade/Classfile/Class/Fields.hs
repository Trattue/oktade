-- |
-- Module: Data.Oktade.Classfile.Class.Fields
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile fields.
module Data.Oktade.Classfile.Class.Fields
  ( -- * Fields
    Fields (..),
    Field (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (count)
import Data.ByteString.Builder (word16BE)
import Data.Oktade.ByteParser (anyWord16)
import Data.Oktade.Classfile.Class.Attributes (Attributes)
import Data.Oktade.Classfile.Class.Fields.AccessFlags (FieldAccessFlags)
import Data.Oktade.Classfile.Class.Parse (Parse (..), Unparse (..))
import Data.Oktade.Classfile.Metadata.ConstantPool (Utf8Ref)
import qualified Data.Oktade.Parse as P (parser, unparser)

--------------------------------------------------------------------------------
-- Fields
--------------------------------------------------------------------------------

-- | List of fields a classfile has.
--
-- Read the JVM specification for more information:
-- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.5
newtype Fields = Fields [Field]
  deriving (Show)

instance Parse Fields where
  parser m = Fields <$> (anyWord16 >>= flip count (parser m) . fromIntegral)

instance Unparse Fields where
  unparser m (Fields fs) =
    word16BE (fromIntegral $ length fs) <> foldr ((<>) . unparser m) mempty fs

-- | A single field.
data Field = Field FieldAccessFlags Utf8Ref Utf8Ref Attributes
  deriving (Show)

instance Parse Field where
  parser m = Field <$> parser m <*> P.parser <*> P.parser <*> parser m

instance Unparse Field where
  unparser m (Field a u u' as) =
    unparser m a <> P.unparser u <> P.unparser u' <> unparser m as

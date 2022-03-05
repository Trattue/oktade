-- |
-- Module: Data.Oktade.Classfile.Class.Methods
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile methods.
module Data.Oktade.Classfile.Class.Methods
  ( -- * Methods
    Methods (..),
    Method (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (count)
import Data.ByteString.Builder (word16BE)
import Data.Oktade.ByteParser (anyWord16)
import Data.Oktade.Classfile.Class.Methods.AccessFlags (MethodAccessFlags)
import Data.Oktade.Classfile.Class.Methods.Attributes (MethodAttributes)
import Data.Oktade.Classfile.Class.Parse (Parse (..), Unparse (..))
import Data.Oktade.Classfile.Metadata.ConstantPool (Utf8Ref)
import qualified Data.Oktade.Parse as P (parser, unparser)

--------------------------------------------------------------------------------
-- Methods
--------------------------------------------------------------------------------

-- | List of methods a classfile has.
--
-- Read the JVM specification for more information:
-- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.6
newtype Methods = Methods [Method]
  deriving (Show)

instance Parse Methods where
  parser m = Methods <$> (anyWord16 >>= flip count (parser m) . fromIntegral)

instance Unparse Methods where
  unparser m (Methods ms) =
    word16BE (fromIntegral $ length ms) <> foldr ((<>) . unparser m) mempty ms

-- | A single method.
data Method = Method MethodAccessFlags Utf8Ref Utf8Ref MethodAttributes
  deriving (Show)

instance Parse Method where
  parser m = Method <$> parser m <*> P.parser <*> P.parser <*> parser m

instance Unparse Method where
  unparser m (Method a u u' as) =
    unparser m a <> P.unparser u <> P.unparser u' <> unparser m as

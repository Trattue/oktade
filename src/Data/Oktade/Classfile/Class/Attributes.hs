-- |
-- Module: Data.Oktade.Classfile.Class.Attributes
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile attributes.
module Data.Oktade.Classfile.Class.Attributes
  ( -- * Attributes
    Attributes (..),
    Attribute (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (count)
import qualified Data.Attoparsec.ByteString.Lazy as A (take)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteString, word16BE, word32BE)
import Data.Oktade.ByteParser (anyWord16, anyWord32)
import Data.Oktade.Classfile.Class.Parse (Parse (..), Unparse (..))
import Data.Oktade.Classfile.Metadata.ConstantPool (Utf8Ref)
import qualified Data.Oktade.Parse as P (parser, unparser)

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

-- | List of fields a field/method/classfile has.
newtype Attributes = Attributes [Attribute]
  deriving (Show)

instance Parse Attributes where
  parser m =
    Attributes <$> do
      attributeCount <- anyWord16
      count (fromIntegral attributeCount) (parser m)

instance Unparse Attributes where
  unparser m (Attributes as) =
    word16BE (fromIntegral $ length as) <> foldr ((<>) . unparser m) mempty as

-- | A single attribute.
data Attribute = Unknown Utf8Ref ByteString
  deriving (Show)

instance Parse Attribute where
  parser _ =
    Unknown <$> P.parser <*> do
      attributeSize <- anyWord32
      A.take (fromIntegral attributeSize)

instance Unparse Attribute where
  unparser _ (Unknown u b) =
    P.unparser u <> word32BE (fromIntegral $ BS.length b) <> byteString b

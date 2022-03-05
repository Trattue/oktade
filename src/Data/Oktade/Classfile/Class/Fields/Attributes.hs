-- |
-- Module: Data.Oktade.Classfile.Class.Fields.Attributes
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile field attributes.
module Data.Oktade.Classfile.Class.Fields.Attributes
  ( -- * Attributes
    FieldAttributes (..),

    -- ** Attribute Names
    NConstantValue (..),
    NSynthetic (..),
    NDeprecated (..),
    NSignature (..),
    NRuntimeVisibleAnnotations (..),
    NRuntimeInvisibleAnnotations (..),
    NRuntimeVisibleTypeAnnotations (..),
    NRuntimeInvisibleTypeAnnotations (..),

    -- ** Field Attributes
    FiledAttribute (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (choice, count)
import qualified Data.Attoparsec.ByteString.Lazy as A (take)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteString, word16BE, word32BE)
import Data.Oktade.ByteParser (anyWord16, anyWord32)
import Data.Oktade.Classfile.Class.Attributes (attrNameParser)
import Data.Oktade.Classfile.Class.Parse (Parse (..), Unparse (..))
import Data.Oktade.Classfile.Metadata.ConstantPool (Utf8Ref (..))
import qualified Data.Oktade.Parse as P (parser, unparser)

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

-- | List of attributes a field has.
--
-- More about the attributes can be learned in the JVM specification:
-- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.7
newtype FieldAttributes = FieldAttributes [FiledAttribute]
  deriving (Show)

instance Parse FieldAttributes where
  parser m =
    FieldAttributes <$> (anyWord16 >>= flip count (parser m) . fromIntegral)

instance Unparse FieldAttributes where
  unparser m (FieldAttributes as) =
    word16BE (fromIntegral $ length as) <> foldr ((<>) . unparser m) mempty as

--------------------------------------------------------------------------------
-- Attribute Names
--------------------------------------------------------------------------------

newtype NConstantValue = NConstantValue Utf8Ref
  deriving (Show)

instance Parse NConstantValue where
  parser m = do
    idx <- anyWord16
    let (Just n) = attrNameParser idx "ConstantValue" m NConstantValue
    return $ n $ Utf8Ref idx

instance Unparse NConstantValue where
  unparser _ (NConstantValue u) = P.unparser u

newtype NSynthetic = NSynthetic Utf8Ref
  deriving (Show)

instance Parse NSynthetic where
  parser m = do
    idx <- anyWord16
    let (Just n) = attrNameParser idx "Synthetic" m NSynthetic
    return $ n $ Utf8Ref idx

instance Unparse NSynthetic where
  unparser _ (NSynthetic u) = P.unparser u

newtype NDeprecated = NDeprecated Utf8Ref
  deriving (Show)

instance Parse NDeprecated where
  parser m = do
    idx <- anyWord16
    let (Just n) = attrNameParser idx "Deprecated" m NDeprecated
    return $ n $ Utf8Ref idx

instance Unparse NDeprecated where
  unparser _ (NDeprecated u) = P.unparser u

newtype NSignature = NSignature Utf8Ref
  deriving (Show)

instance Parse NSignature where
  parser m = do
    idx <- anyWord16
    let (Just n) = attrNameParser idx "Signature" m NSignature
    return $ n $ Utf8Ref idx

instance Unparse NSignature where
  unparser _ (NSignature u) = P.unparser u

newtype NRuntimeVisibleAnnotations = NRuntimeVisibleAnnotations Utf8Ref
  deriving (Show)

instance Parse NRuntimeVisibleAnnotations where
  parser m = do
    idx <- anyWord16
    let (Just n) =
          attrNameParser
            idx
            "RuntimeVisibleAnnotations"
            m
            NRuntimeVisibleAnnotations
    return $ n $ Utf8Ref idx

instance Unparse NRuntimeVisibleAnnotations where
  unparser _ (NRuntimeVisibleAnnotations u) = P.unparser u

newtype NRuntimeInvisibleAnnotations = NRuntimeInvisibleAnnotations Utf8Ref
  deriving (Show)

instance Parse NRuntimeInvisibleAnnotations where
  parser m = do
    idx <- anyWord16
    let (Just n) =
          attrNameParser
            idx
            "RuntimeInvisibleAnnotations"
            m
            NRuntimeInvisibleAnnotations
    return $ n $ Utf8Ref idx

instance Unparse NRuntimeInvisibleAnnotations where
  unparser _ (NRuntimeInvisibleAnnotations u) = P.unparser u

newtype NRuntimeVisibleTypeAnnotations = NRuntimeVisibleTypeAnnotations Utf8Ref
  deriving (Show)

instance Parse NRuntimeVisibleTypeAnnotations where
  parser m = do
    idx <- anyWord16
    let (Just n) =
          attrNameParser
            idx
            "RuntimeVisibleTypeAnnotations"
            m
            NRuntimeVisibleTypeAnnotations
    return $ n $ Utf8Ref idx

instance Unparse NRuntimeVisibleTypeAnnotations where
  unparser _ (NRuntimeVisibleTypeAnnotations u) = P.unparser u

newtype NRuntimeInvisibleTypeAnnotations
  = NRuntimeInvisibleTypeAnnotations Utf8Ref
  deriving (Show)

instance Parse NRuntimeInvisibleTypeAnnotations where
  parser m = do
    idx <- anyWord16
    let (Just n) =
          attrNameParser
            idx
            "RuntimeInvisibleTypeAnnotations"
            m
            NRuntimeInvisibleTypeAnnotations
    return $ n $ Utf8Ref idx

instance Unparse NRuntimeInvisibleTypeAnnotations where
  unparser _ (NRuntimeInvisibleTypeAnnotations u) = P.unparser u

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

-- | A single attribute.
data FiledAttribute
  = -- | Unknown attribute.
    Unknown Utf8Ref ByteString
  deriving (Show)

instance Parse FiledAttribute where
  parser _ =
    let parsers = [parserUnknown]
     in choice parsers
    where
      parserUnknown =
        Unknown <$> P.parser <*> (anyWord32 >>= A.take . fromIntegral)

instance Unparse FiledAttribute where
  unparser _ (Unknown u b) =
    P.unparser u <> word32BE (fromIntegral $ BS.length b) <> byteString b

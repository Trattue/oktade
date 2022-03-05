-- |
-- Module: Data.Oktade.Classfile.Class.Methods.Code.Attributes
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile code attribute
-- attributes.
module Data.Oktade.Classfile.Class.Methods.Code.Attributes
  ( -- * Attributes
    CodeAttributes (..),

    -- ** Attribute Names
    NLineNumberTable (..),
    NLocalVariableTable (..),
    NLocalVariableTypeTable (..),
    NStackMapTable (..),
    NRuntimeVisibleTypeAnnotations (..),
    NRuntimeInvisibleTypeAnnotations (..),

    -- ** Method Attributes
    CodeAttribute (..),
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
newtype CodeAttributes = CodeAttributes [CodeAttribute]
  deriving (Show)

instance Parse CodeAttributes where
  parser m =
    CodeAttributes <$> (anyWord16 >>= flip count (parser m) . fromIntegral)

instance Unparse CodeAttributes where
  unparser m (CodeAttributes as) =
    word16BE (fromIntegral $ length as) <> foldr ((<>) . unparser m) mempty as

--------------------------------------------------------------------------------
-- Attribute Names
--------------------------------------------------------------------------------

newtype NLineNumberTable = NLineNumberTable Utf8Ref
  deriving (Show)

instance Parse NLineNumberTable where
  parser m = do
    idx <- anyWord16
    let (Just n) = attrNameParser idx "LineNumberTable" m NLineNumberTable
    return $ n $ Utf8Ref idx

instance Unparse NLineNumberTable where
  unparser _ (NLineNumberTable u) = P.unparser u

newtype NLocalVariableTable = NLocalVariableTable Utf8Ref
  deriving (Show)

instance Parse NLocalVariableTable where
  parser m = do
    idx <- anyWord16
    let (Just n) =
          attrNameParser idx "LocalVariableTable" m NLocalVariableTable
    return $ n $ Utf8Ref idx

instance Unparse NLocalVariableTable where
  unparser _ (NLocalVariableTable u) = P.unparser u

newtype NLocalVariableTypeTable = NLocalVariableTypeTable Utf8Ref
  deriving (Show)

instance Parse NLocalVariableTypeTable where
  parser m = do
    idx <- anyWord16
    let (Just n) =
          attrNameParser idx "LocalVariableTypeTable" m NLocalVariableTypeTable
    return $ n $ Utf8Ref idx

instance Unparse NLocalVariableTypeTable where
  unparser _ (NLocalVariableTypeTable u) = P.unparser u

newtype NStackMapTable = NStackMapTable Utf8Ref
  deriving (Show)

instance Parse NStackMapTable where
  parser m = do
    idx <- anyWord16
    let (Just n) = attrNameParser idx "StackMapTable" m NStackMapTable
    return $ n $ Utf8Ref idx

instance Unparse NStackMapTable where
  unparser _ (NStackMapTable u) = P.unparser u

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
data CodeAttribute
  = -- | Unknown attribute.
    Unknown Utf8Ref ByteString
  deriving (Show)

instance Parse CodeAttribute where
  parser _ =
    let parsers = [parserUnknown]
     in choice parsers
    where
      parserUnknown =
        Unknown <$> P.parser <*> (anyWord32 >>= A.take . fromIntegral)

instance Unparse CodeAttribute where
  unparser _ (Unknown u b) =
    P.unparser u <> word32BE (fromIntegral $ BS.length b) <> byteString b

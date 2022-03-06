-- |
-- Module: Data.Oktade.Classfile.Class.Methods.Attributes
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile methods
-- attributes.
module Data.Oktade.Classfile.Class.Methods.Attributes
  ( -- * Attributes
    MethodAttributes (..),

    -- ** Attribute Names
    NCode (..),
    NExceptions (..),
    NRuntimeVisibleParameterAnnotations (..),
    NRuntimeInvisibleParameterAnnotations (..),
    NAnnotationDefault (..),
    NMethodParameters (..),

    -- ** Method Attributes
    MethodAttribute (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (choice, count)
import qualified Data.Attoparsec.ByteString.Lazy as A (take)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteString, word16BE, word32BE)
import Data.Oktade.ByteParser (anyWord16, anyWord32)
import Data.Oktade.Classfile.Class.Attributes (attrNameParser, checkAttrName)
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
newtype MethodAttributes = MethodAttributes [MethodAttribute]
  deriving (Show)

instance Parse MethodAttributes where
  parser m =
    MethodAttributes <$> (anyWord16 >>= flip count (parser m) . fromIntegral)

instance Unparse MethodAttributes where
  unparser m (MethodAttributes as) =
    word16BE (fromIntegral $ length as) <> foldr ((<>) . unparser m) mempty as

--------------------------------------------------------------------------------
-- Attribute Names
--------------------------------------------------------------------------------

newtype NCode = NCode Utf8Ref
  deriving (Show)

instance Parse NCode where
  parser m = do
    idx <- anyWord16
    n <- checkAttrName $ attrNameParser idx "Code" m NCode
    return $ n $ Utf8Ref idx

instance Unparse NCode where
  unparser _ (NCode u) = P.unparser u

newtype NExceptions = NExceptions Utf8Ref
  deriving (Show)

instance Parse NExceptions where
  parser m = do
    idx <- anyWord16
    n <- checkAttrName $ attrNameParser idx "Exceptions" m NExceptions
    return $ n $ Utf8Ref idx

instance Unparse NExceptions where
  unparser _ (NExceptions u) = P.unparser u

newtype NRuntimeVisibleParameterAnnotations
  = NRuntimeVisibleParameterAnnotations Utf8Ref
  deriving (Show)

instance Parse NRuntimeVisibleParameterAnnotations where
  parser m = do
    idx <- anyWord16
    n <-
      checkAttrName $
        attrNameParser
          idx
          "RuntimeVisibleParameterAnnotations"
          m
          NRuntimeVisibleParameterAnnotations
    return $ n $ Utf8Ref idx

instance Unparse NRuntimeVisibleParameterAnnotations where
  unparser _ (NRuntimeVisibleParameterAnnotations u) = P.unparser u

newtype NRuntimeInvisibleParameterAnnotations
  = NRuntimeInvisibleParameterAnnotations Utf8Ref
  deriving (Show)

instance Parse NRuntimeInvisibleParameterAnnotations where
  parser m = do
    idx <- anyWord16
    n <-
      checkAttrName $
        attrNameParser
          idx
          "RuntimeInvisibleParameterAnnotations"
          m
          NRuntimeInvisibleParameterAnnotations
    return $ n $ Utf8Ref idx

instance Unparse NRuntimeInvisibleParameterAnnotations where
  unparser _ (NRuntimeInvisibleParameterAnnotations u) = P.unparser u

newtype NAnnotationDefault = NAnnotationDefault Utf8Ref
  deriving (Show)

instance Parse NAnnotationDefault where
  parser m = do
    idx <- anyWord16
    n <-
      checkAttrName $
        attrNameParser idx "AnnotationDefault" m NAnnotationDefault
    return $ n $ Utf8Ref idx

instance Unparse NAnnotationDefault where
  unparser _ (NAnnotationDefault u) = P.unparser u

newtype NMethodParameters = NMethodParameters Utf8Ref
  deriving (Show)

instance Parse NMethodParameters where
  parser m = do
    idx <- anyWord16
    n <-
      checkAttrName $ attrNameParser idx "MethodParameters" m NMethodParameters
    return $ n $ Utf8Ref idx

instance Unparse NMethodParameters where
  unparser _ (NMethodParameters u) = P.unparser u

newtype NSynthetic = NSynthetic Utf8Ref
  deriving (Show)

instance Parse NSynthetic where
  parser m = do
    idx <- anyWord16
    n <- checkAttrName $ attrNameParser idx "Synthetic" m NSynthetic
    return $ n $ Utf8Ref idx

instance Unparse NSynthetic where
  unparser _ (NSynthetic u) = P.unparser u

newtype NDeprecated = NDeprecated Utf8Ref
  deriving (Show)

instance Parse NDeprecated where
  parser m = do
    idx <- anyWord16
    n <- checkAttrName $ attrNameParser idx "Deprecated" m NDeprecated
    return $ n $ Utf8Ref idx

instance Unparse NDeprecated where
  unparser _ (NDeprecated u) = P.unparser u

newtype NSignature = NSignature Utf8Ref
  deriving (Show)

instance Parse NSignature where
  parser m = do
    idx <- anyWord16
    n <- checkAttrName $ attrNameParser idx "Signature" m NSignature
    return $ n $ Utf8Ref idx

instance Unparse NSignature where
  unparser _ (NSignature u) = P.unparser u

newtype NRuntimeVisibleAnnotations = NRuntimeVisibleAnnotations Utf8Ref
  deriving (Show)

instance Parse NRuntimeVisibleAnnotations where
  parser m = do
    idx <- anyWord16
    n <-
      checkAttrName $
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
    n <-
      checkAttrName $
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
    n <-
      checkAttrName $
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
    n <-
      checkAttrName $
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
data MethodAttribute
  = -- | Method is synthetic.
    Synthetic NSynthetic
  | -- | Unknown attribute.
    Unknown Utf8Ref ByteString
  deriving (Show)

instance Parse MethodAttribute where
  parser m =
    let parsers =
          [ parserUnknown,
            parserSynthetic
          ]
     in choice parsers
    where
      parserSynthetic =
        Synthetic <$> do
          n <- parser m
          _ <- anyWord32
          return n
      parserUnknown =
        Unknown <$> P.parser <*> (anyWord32 >>= A.take . fromIntegral)

instance Unparse MethodAttribute where
  unparser m (Synthetic n) = unparser m n <> word32BE 0
  unparser _ (Unknown u b) =
    P.unparser u <> word32BE (fromIntegral $ BS.length b) <> byteString b

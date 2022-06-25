-- |
-- Module: Data.Oktade.Classfile.Class.Attributes
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile attributes.
module Data.Oktade.Classfile.Class.Attributes
  ( -- * Attributes
    Attributes (..),

    -- ** Class Attributes
    Attribute (..),
    InnerClass (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (count)
import qualified Data.Attoparsec.ByteString.Lazy as A (take)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, byteString, word16BE, word32BE)
import Data.ByteString.Char8 (pack, unpack)
import Data.IntMap (lookupMin, (!?))
import qualified Data.IntMap as IntMap (filter)
import Data.Oktade.ByteParser (anyWord16, anyWord32)
import Data.Oktade.Classfile.Class.Parse (Parse (..), Unparse (..))
import Data.Oktade.Classfile.Metadata (Metadata (constantPool))
import Data.Oktade.Classfile.Metadata.ConstantPool
  ( ConstantPool (..),
    ConstantPoolEntry (Utf8),
    Utf8Ref (..),
  )
import qualified Data.Oktade.Parse as P (Parse (parser), unparser)
import Data.Word (Word16)

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

-- | List of fields a class has.
--
-- More about the attributes can be learned in the JVM specification:
-- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.7
newtype Attributes = Attributes [Attribute]
  deriving (Show)

instance Parse Attributes where
  parser m = Attributes <$> (anyWord16 >>= flip count (parser m) . fromIntegral)

instance Unparse Attributes where
  unparser m (Attributes as) =
    word16BE (fromIntegral $ length as) <> foldr ((<>) . unparser m) mempty as

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

-- | A single attribute.
data Attribute
  = SourceFile Utf8Ref
  | InnerClasses [InnerClass]
  | Unknown Utf8Ref ByteString
  deriving (Show)

instance Parse Attribute where
  parser m = anyWord16 >>= parser'
    where
      parser' i = do
        case entries (constantPool m) !? fromIntegral i of
          Just (Utf8 bs) -> parser'' (unpack bs) i
          _ -> error "Invalid constant pool reference in attribute name"
      parser'' n i
        | n == sourceFileName = parserSourceFile
        | n == innerClassesName = parserInnerClasses
        | n == enclosingMethodName = parserUnknown i
        | n == sourceDebugExtensionName = parserUnknown i
        | n == bootstrapMethodsName = parserUnknown i
        | n == moduleName = parserUnknown i
        | n == modulePackagesName = parserUnknown i
        | n == moduleMainClassName = parserUnknown i
        | n == nestHostName = parserUnknown i
        | n == nestMembersName = parserUnknown i
        | n == recordName = parserUnknown i
        | n == permittedSubclassesName = parserUnknown i
        | n == syntheticName = parserUnknown i
        | n == deprecatedName = parserUnknown i
        | n == signatureName = parserUnknown i
        | n == runtimeVisibleAnnotationsName = parserUnknown i
        | n == runtimeInvisibleAnnotationsName = parserUnknown i
        | n == runtimeVisibleTypeAnnotationsName = parserUnknown i
        | n == runtimeInvisibleTypeAnnotationsName = parserUnknown i
        | otherwise = parserUnknown i
      parserSourceFile = anyWord32 >> SourceFile <$> P.parser
      parserInnerClasses =
        anyWord32 >> InnerClasses <$> (anyWord16 >>= parseMultiple parser)
      parserUnknown i =
        Unknown (Utf8Ref i) <$> (anyWord32 >>= A.take . fromIntegral)
      parseMultiple p i = count (fromIntegral i) (p m)

instance Unparse Attribute where
  unparser m (SourceFile u) =
    nameUnparser sourceFileName m <> word32BE 2 <> P.unparser u
  unparser m (InnerClasses is) =
    nameUnparser innerClassesName m
      <> word32BE (fromIntegral $ 2 + 8 * length is)
      <> word16BE (fromIntegral $ length is)
      <> foldr ((<>) . unparser m) mempty is
  unparser _ (Unknown u b) =
    P.unparser u <> word32BE (fromIntegral $ BS.length b) <> byteString b

data InnerClass = InnerClass Utf8Ref Utf8Ref Utf8Ref Word16 -- TODO: access flags
  deriving (Show)

instance Parse InnerClass where
  parser _ = InnerClass <$> P.parser <*> P.parser <*> P.parser <*> anyWord16

instance Unparse InnerClass where
  unparser _ (InnerClass i o n f) =
    P.unparser i <> P.unparser o <> P.unparser n <> word16BE f

nameUnparser :: String -> Metadata -> Builder
nameUnparser n m = do
  let u = Utf8 $ pack n
  let b = IntMap.filter (== u) $ entries $ constantPool m
  case lookupMin b of
    Just (i, _) -> word16BE $ fromIntegral i
    Nothing -> error "not implemented" -- TODO: insert

sourceFileName :: String
sourceFileName = "SourceFile"

innerClassesName :: String
innerClassesName = "InnerClasses"

enclosingMethodName :: String
enclosingMethodName = "EnclosingMethod"

sourceDebugExtensionName :: String
sourceDebugExtensionName = "SourceDebugExtension"

bootstrapMethodsName :: String
bootstrapMethodsName = "BootstrapMethods"

moduleName :: String
moduleName = "Module"

modulePackagesName :: String
modulePackagesName = "ModulePackages"

moduleMainClassName :: String
moduleMainClassName = "ModuleMainClass"

nestHostName :: String
nestHostName = "NestHost"

nestMembersName :: String
nestMembersName = "NestMembers"

recordName :: String
recordName = "Record"

permittedSubclassesName :: String
permittedSubclassesName = "PermittedSubclasses"

syntheticName :: String
syntheticName = "Synthetic"

deprecatedName :: String
deprecatedName = "Deprecated"

signatureName :: String
signatureName = "Signature"

runtimeVisibleAnnotationsName :: String
runtimeVisibleAnnotationsName = "RuntimeVisibleAnnotations"

runtimeInvisibleAnnotationsName :: String
runtimeInvisibleAnnotationsName = "RuntimeInvisibleAnnotations"

runtimeVisibleTypeAnnotationsName :: String
runtimeVisibleTypeAnnotationsName = "RuntimeVisibleTypeAnnotations"

runtimeInvisibleTypeAnnotationsName :: String
runtimeInvisibleTypeAnnotationsName = "RuntimeInvisibleTypeAnnotations"

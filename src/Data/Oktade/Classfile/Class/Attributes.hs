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
  ( ClassRef,
    ConstantPool (..),
    ConstantPoolEntry (Utf8),
    NameAndTypeRef,
    PackageRef,
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
  = -- | Name of the original source file.
    SourceFile Utf8Ref
  | -- | Inner classes of the class.
    InnerClasses [InnerClass]
  | -- | Enclosing method of a local or anonymous class.
    EnclosingMethod ClassRef NameAndTypeRef
  | -- | "Extended debug information", whatever that may be.
    SourceDebugExtension ByteString
  | ModulePackages [PackageRef]
  | ModuleMainClass ClassRef
  | NestHost ClassRef
  | NestMembers [ClassRef]
  | PermittedSubclasses [ClassRef]
  | -- | Class is synthetic.
    Synthetic
  | -- | Class is deprecated.
    Deprecated
  | Signature Utf8Ref
  | -- | Unknown attribute.
    Unknown Utf8Ref ByteString
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
        | n == enclosingMethodName = parserEnclosingMethod
        | n == sourceDebugExtensionName = parserSourceDebugExtension
        | n == bootstrapMethodsName = parserUnknown i -- TODO
        | n == moduleName = parserUnknown i -- TODO
        | n == modulePackagesName = parserModulePackages
        | n == moduleMainClassName = parserModuleMainClass
        | n == nestHostName = parserNestHost
        | n == nestMembersName = parserNestMembers
        | n == recordName = parserUnknown i -- TODO
        | n == permittedSubclassesName = parserPermittedSubclasses
        | n == syntheticName = parserSynthetic
        | n == deprecatedName = parserDeprecated
        | n == signatureName = parserSignature
        | n == runtimeVisibleAnnotationsName = parserUnknown i -- TODO
        | n == runtimeInvisibleAnnotationsName = parserUnknown i -- TODO
        | n == runtimeVisibleTypeAnnotationsName = parserUnknown i -- TODO
        | n == runtimeInvisibleTypeAnnotationsName = parserUnknown i -- TODO
        | otherwise = parserUnknown i -- TODO
      parserSourceFile = anyWord32 >> SourceFile <$> P.parser
      parserInnerClasses =
        anyWord32 >> InnerClasses <$> (anyWord16 >>= parseMultiple (parser m))
      parserEnclosingMethod =
        anyWord32 >> EnclosingMethod <$> P.parser <*> P.parser
      parserSourceDebugExtension =
        SourceDebugExtension <$> (anyWord32 >>= A.take . fromIntegral)
      parserModulePackages =
        anyWord32 >> ModulePackages <$> (anyWord16 >>= parseMultiple P.parser)
      parserModuleMainClass = anyWord32 >> ModuleMainClass <$> P.parser
      parserNestHost = anyWord32 >> NestHost <$> P.parser
      parserNestMembers =
        anyWord32 >> NestMembers <$> (anyWord16 >>= parseMultiple P.parser)
      parserPermittedSubclasses =
        anyWord32
          >> PermittedSubclasses <$> (anyWord16 >>= parseMultiple P.parser)
      parserSynthetic = anyWord32 >> pure Synthetic
      parserDeprecated = anyWord32 >> pure Deprecated
      parserSignature = anyWord32 >> Signature <$> P.parser
      parserUnknown i =
        Unknown (Utf8Ref i) <$> (anyWord32 >>= A.take . fromIntegral)
      parseMultiple p i = count (fromIntegral i) p

instance Unparse Attribute where
  unparser m (SourceFile u) =
    nameUnparser sourceFileName m <> word32BE 2 <> P.unparser u
  unparser m (InnerClasses is) =
    nameUnparser innerClassesName m
      <> word32BE (fromIntegral $ 2 + 8 * length is)
      <> word16BE (fromIntegral $ length is)
      <> foldr ((<>) . unparser m) mempty is
  unparser m (EnclosingMethod c n) =
    nameUnparser enclosingMethodName m
      <> word32BE 4
      <> P.unparser c
      <> P.unparser n
  unparser m (SourceDebugExtension bs) =
    nameUnparser sourceDebugExtensionName m
      <> word32BE (fromIntegral $ BS.length bs)
      <> byteString bs
  unparser m (ModulePackages ps) =
    nameUnparser modulePackagesName m
      <> word32BE (fromIntegral $ 2 + 2 * length ps)
      <> word16BE (fromIntegral $ length ps)
      <> foldr ((<>) . P.unparser) mempty ps
  unparser m (ModuleMainClass c) =
    nameUnparser moduleMainClassName m <> word32BE 2 <> P.unparser c
  unparser m (NestHost c) =
    nameUnparser nestHostName m <> word32BE 2 <> P.unparser c
  unparser m (NestMembers cs) =
    nameUnparser nestMembersName m
      <> word32BE (fromIntegral $ 2 + 2 * length cs)
      <> word16BE (fromIntegral $ length cs)
      <> foldr ((<>) . P.unparser) mempty cs
  unparser m (PermittedSubclasses cs) =
    nameUnparser permittedSubclassesName m
      <> word32BE (fromIntegral $ 2 + 2 * length cs)
      <> word16BE (fromIntegral $ length cs)
      <> foldr ((<>) . P.unparser) mempty cs
  unparser m Synthetic = nameUnparser syntheticName m <> word32BE 2
  unparser m Deprecated = nameUnparser deprecatedName m <> word32BE 2
  unparser m (Signature u) =
    nameUnparser signatureName m <> word32BE 2 <> P.unparser u
  unparser _ (Unknown u bs) =
    P.unparser u <> word32BE (fromIntegral $ BS.length bs) <> byteString bs

data InnerClass = InnerClass ClassRef ClassRef Utf8Ref Word16 -- TODO: access flags
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
    Nothing -> error $ "could not find " ++ n -- TODO: insert

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

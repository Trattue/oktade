-- |
-- Module: Data.Oktade.Classfile.Class.Attributes
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile attributes.
module Data.Oktade.Classfile.Class.Attributes
  ( -- * Attributes
    Attributes (..),

    -- ** Attribute Names
    attrNameParser,
    checkAttrName,
    NSourceFile (..),
    NInnerClasses (..),
    NEnclosingMethod (..),
    NSourceDebugExtension (..),
    NBootstrapMethods (..),
    NModule (..),
    NModulePackages (..),
    NModuleMainClass (..),
    NNestHost (..),
    NNestMembers (..),
    NRecord (..),
    NPermittedSubclasses (..),
    NSynthetic (..),
    NDeprecated (..),
    NSignature (..),
    NRuntimeVisibleAnnotations (..),
    NRuntimeInvisibleAnnotations (..),
    NRuntimeVisibleTypeAnnotations (..),
    NRuntimeInvisibleTypeAnnotations (..),

    -- ** Class Attributes
    Attribute (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (choice, count)
import qualified Data.Attoparsec.ByteString.Lazy as A (take)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteString, word16BE, word32BE)
import Data.IntMap ((!?))
import Data.Oktade.ByteParser (anyWord16, anyWord32)
import Data.Oktade.Classfile.Class.Parse (Parse (..), Unparse (..))
import Data.Oktade.Classfile.Metadata (Metadata (constantPool))
import Data.Oktade.Classfile.Metadata.ConstantPool
  ( ClassRef,
    ConstantPool (..),
    ConstantPoolEntry (Utf8),
    NameAndTypeRef,
    Utf8Ref (..),
  )
import qualified Data.Oktade.Parse as P (parser, unparser)
import Data.String (fromString)
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
-- Attribute Names
--------------------------------------------------------------------------------

attrNameParser :: Word16 -> String -> Metadata -> a -> Maybe a
attrNameParser i n m a = do
  let (Just (Utf8 bs)) = entries (constantPool m) !? fromIntegral i
  if bs == fromString n
    then Just a
    else Nothing

checkAttrName :: MonadFail m => Maybe a -> m a
checkAttrName (Just n) = return n
checkAttrName _ = fail ""

newtype NSourceFile = NSourceFile Utf8Ref
  deriving (Show)

instance Parse NSourceFile where
  parser m = do
    idx <- anyWord16
    n <- checkAttrName $ attrNameParser idx "SourceFile" m NSourceFile
    return $ n $ Utf8Ref idx

instance Unparse NSourceFile where
  unparser _ (NSourceFile u) = P.unparser u

newtype NInnerClasses = NInnerClasses Utf8Ref
  deriving (Show)

instance Parse NInnerClasses where
  parser m = do
    idx <- anyWord16
    n <- checkAttrName $ attrNameParser idx "InnerClasses" m NInnerClasses
    return $ n $ Utf8Ref idx

instance Unparse NInnerClasses where
  unparser _ (NInnerClasses u) = P.unparser u

newtype NEnclosingMethod = NEnclosingMethod Utf8Ref
  deriving (Show)

instance Parse NEnclosingMethod where
  parser m = do
    idx <- anyWord16
    n <- checkAttrName $ attrNameParser idx "EnclosingMethod" m NEnclosingMethod
    return $ n $ Utf8Ref idx

instance Unparse NEnclosingMethod where
  unparser _ (NEnclosingMethod u) = P.unparser u

newtype NSourceDebugExtension = NSourceDebugExtension Utf8Ref
  deriving (Show)

instance Parse NSourceDebugExtension where
  parser m = do
    idx <- anyWord16
    n <-
      checkAttrName $
        attrNameParser idx "SourceDebugExtension" m NSourceDebugExtension
    return $ n $ Utf8Ref idx

instance Unparse NSourceDebugExtension where
  unparser _ (NSourceDebugExtension u) = P.unparser u

newtype NBootstrapMethods = NBootstrapMethods Utf8Ref
  deriving (Show)

instance Parse NBootstrapMethods where
  parser m = do
    idx <- anyWord16
    n <-
      checkAttrName $ attrNameParser idx "BootstrapMethods" m NBootstrapMethods
    return $ n $ Utf8Ref idx

instance Unparse NBootstrapMethods where
  unparser _ (NBootstrapMethods u) = P.unparser u

newtype NModule = NModule Utf8Ref
  deriving (Show)

instance Parse NModule where
  parser m = do
    idx <- anyWord16
    n <- checkAttrName $ attrNameParser idx "Module" m NModule
    return $ n $ Utf8Ref idx

instance Unparse NModule where
  unparser _ (NModule u) = P.unparser u

newtype NModulePackages = NModulePackages Utf8Ref
  deriving (Show)

instance Parse NModulePackages where
  parser m = do
    idx <- anyWord16
    n <- checkAttrName $ attrNameParser idx "ModulePackages" m NModulePackages
    return $ n $ Utf8Ref idx

instance Unparse NModulePackages where
  unparser _ (NModulePackages u) = P.unparser u

newtype NModuleMainClass = NModuleMainClass Utf8Ref
  deriving (Show)

instance Parse NModuleMainClass where
  parser m = do
    idx <- anyWord16
    n <- checkAttrName $ attrNameParser idx "ModuleMainClass" m NModuleMainClass
    return $ n $ Utf8Ref idx

instance Unparse NModuleMainClass where
  unparser _ (NModuleMainClass u) = P.unparser u

newtype NNestHost = NNestHost Utf8Ref
  deriving (Show)

instance Parse NNestHost where
  parser m = do
    idx <- anyWord16
    n <- checkAttrName $ attrNameParser idx "NestHost" m NNestHost
    return $ n $ Utf8Ref idx

instance Unparse NNestHost where
  unparser _ (NNestHost u) = P.unparser u

newtype NNestMembers = NNestMembers Utf8Ref
  deriving (Show)

instance Parse NNestMembers where
  parser m = do
    idx <- anyWord16
    n <- checkAttrName $ attrNameParser idx "NestMembers" m NNestMembers
    return $ n $ Utf8Ref idx

instance Unparse NNestMembers where
  unparser _ (NNestMembers u) = P.unparser u

newtype NRecord = NRecord Utf8Ref
  deriving (Show)

instance Parse NRecord where
  parser m = do
    idx <- anyWord16
    n <- checkAttrName $ attrNameParser idx "Record" m NRecord
    return $ n $ Utf8Ref idx

instance Unparse NRecord where
  unparser _ (NRecord u) = P.unparser u

newtype NPermittedSubclasses = NPermittedSubclasses Utf8Ref
  deriving (Show)

instance Parse NPermittedSubclasses where
  parser m = do
    idx <- anyWord16
    n <-
      checkAttrName $
        attrNameParser idx "PermittedSubclasses" m NPermittedSubclasses
    return $ n $ Utf8Ref idx

instance Unparse NPermittedSubclasses where
  unparser _ (NPermittedSubclasses u) = P.unparser u

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
data Attribute
  = -- | Name of the original source file.
    SourceFile NSourceFile Utf8Ref
  | -- | Enclosing method of a local or anonymous class.
    EnclosingMethod NEnclosingMethod ClassRef NameAndTypeRef
  | -- | Class is synthetic.
    Synthetic NSynthetic
  | -- | Unknown attribute.
    Unknown Utf8Ref ByteString
  deriving (Show)

instance Parse Attribute where
  parser m =
    let parsers =
          [ parserSourceFile,
            parserUnknown,
            parserEnclosingMethod,
            parserSynthetic
          ]
     in choice parsers
    where
      parserSourceFile = SourceFile <$> parser m <*> (anyWord32 >> P.parser)
      parserEnclosingMethod =
        EnclosingMethod <$> parser m <*> (anyWord32 >> P.parser) <*> P.parser
      parserSynthetic =
        Synthetic <$> do
          n <- parser m
          _ <- anyWord32
          return n
      parserUnknown =
        Unknown <$> P.parser <*> (anyWord32 >>= A.take . fromIntegral)

instance Unparse Attribute where
  unparser m (SourceFile n u) = unparser m n <> word32BE 2 <> P.unparser u
  unparser m (EnclosingMethod n c n') =
    unparser m n <> word32BE 4 <> P.unparser c <> P.unparser n'
  unparser m (Synthetic n) = unparser m n <> word32BE 0
  unparser _ (Unknown u b) =
    P.unparser u <> word32BE (fromIntegral $ BS.length b) <> byteString b

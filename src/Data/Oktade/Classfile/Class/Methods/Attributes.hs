-- |
-- Module: Data.Oktade.Classfile.Class.Methods.Attributes
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile methods
-- attributes.
module Data.Oktade.Classfile.Class.Methods.Attributes
  ( -- * Attributes
    MethodAttributes (..),

    -- ** Method Attributes
    MethodAttribute (..),
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
  ( ConstantPool (entries),
    ConstantPoolEntry (Utf8),
    Utf8Ref (..),
  )
import qualified Data.Oktade.Parse as P (unparser)

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
-- Attributes
--------------------------------------------------------------------------------

-- | A single attribute.
data MethodAttribute = Unknown Utf8Ref ByteString
  deriving (Show)

instance Parse MethodAttribute where
  parser m = anyWord16 >>= parser'
    where
      parser' i = do
        case entries (constantPool m) !? fromIntegral i of
          Just (Utf8 bs) -> parser'' (unpack bs) i
          _ -> error "Invalid constant pool reference in attribute name"
      parser'' n i
        | n == codeName = parserUnknown i
        | otherwise = parserUnknown i
      parserUnknown i =
        Unknown (Utf8Ref i) <$> (anyWord32 >>= A.take . fromIntegral)

instance Unparse MethodAttribute where
  unparser _ (Unknown u b) =
    P.unparser u <> word32BE (fromIntegral $ BS.length b) <> byteString b

nameUnparser :: String -> Metadata -> Builder
nameUnparser n m = do
  let u = Utf8 $ pack n
  let b = IntMap.filter (== u) $ entries $ constantPool m
  case lookupMin b of
    Just (i, _) -> word16BE $ fromIntegral i
    Nothing -> error ""

codeName :: String
codeName = "Code"

exceptionsName :: String
exceptionsName = "Exceptions"

runtimeVisibleParameterAnnotationsName :: String
runtimeVisibleParameterAnnotationsName = "RuntimeVisibleParameterAnnotations"

runtimeInvisibleParameterAnnotationsName :: String
runtimeInvisibleParameterAnnotationsName = "RuntimeInvisibleParameterAnnotations"

annotationDefaultName :: String
annotationDefaultName = "AnnotationDefault"

methodParametersName :: String
methodParametersName = "MethodParameters"

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

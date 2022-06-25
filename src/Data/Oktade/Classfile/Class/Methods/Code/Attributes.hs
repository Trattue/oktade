-- |
-- Module: Data.Oktade.Classfile.Class.Methods.Code.Attributes
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile code attribute
-- attributes.
module Data.Oktade.Classfile.Class.Methods.Code.Attributes
  ( -- * Attributes
    CodeAttributes (..),

    -- ** Method Attributes
    CodeAttribute (..),
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
  ( ConstantPoolEntry (Utf8),
    Utf8Ref (..),
    entries,
  )
import qualified Data.Oktade.Parse as P (unparser)

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
-- Attributes
--------------------------------------------------------------------------------

-- | A single attribute.
data CodeAttribute
  = -- | Unknown attribute.
    Unknown Utf8Ref ByteString
  deriving (Show)

instance Parse CodeAttribute where
  parser m = anyWord16 >>= parser'
    where
      parser' i = do
        case entries (constantPool m) !? fromIntegral i of
          Just (Utf8 bs) -> parser'' (unpack bs) i
          _ -> error "Invalid constant pool reference in attribute name"
      parser'' n i
        | n == lineNumberTableName = parserUnknown i
        | otherwise = parserUnknown i
      parserUnknown i =
        Unknown (Utf8Ref i) <$> (anyWord32 >>= A.take . fromIntegral)

instance Unparse CodeAttribute where
  unparser _ (Unknown u b) =
    P.unparser u <> word32BE (fromIntegral $ BS.length b) <> byteString b

nameUnparser :: String -> Metadata -> Builder
nameUnparser n m = do
  let u = Utf8 $ pack n
  let b = IntMap.filter (== u) $ entries $ constantPool m
  case lookupMin b of
    Just (i, _) -> word16BE $ fromIntegral i
    Nothing -> error ""

lineNumberTableName :: String
lineNumberTableName = "LineNumberTable"

localVariableTableName :: String
localVariableTableName = "LocalVariableTable"

localVariableTypeTableName :: String
localVariableTypeTableName = "LocalVariableTypeTable"

stackMapTableName :: String
stackMapTableName = "StackMapTable"

runtimeVisibleTypeAnnotationsName :: String
runtimeVisibleTypeAnnotationsName = "RuntimeVisibleTypeAnnotations"

runtimeInvisibleTypeAnnotationsName :: String
runtimeInvisibleTypeAnnotationsName = "RuntimeInvisibleTypeAnnotations"

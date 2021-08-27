-- |
-- Module      : Data.Oktade.Classfile.Attributes
-- License     : Apache-2.0
--
-- Type definitions for the classfile attributes.
module Data.Oktade.Classfile.Attributes
  ( Attributes (..),
    Attribute (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (count)
import qualified Data.Attoparsec.ByteString.Lazy as A (take)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (length)
import Data.ByteString.Builder (byteString, word16BE, word32BE)
import Data.Oktade.ByteParser (anyWord16, anyWord32)
import Data.Oktade.Classfile.ConstantPool (Utf8Ref)
import Data.Oktade.Component (Component (..))

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

-- | Represents the list of fields a field/method/classfile has.
newtype Attributes = Attributes [Attribute]

instance Show Attributes where
  show (Attributes []) = "Attributes: -"
  show (Attributes as) =
    "Attributes:\n" ++ init (unlines $ ("  " ++) . show <$> as)

instance Component Attributes where
  parser =
    Attributes <$> do
      attributeCount <- anyWord16
      count (fromIntegral attributeCount) parser
  encode (Attributes as) =
    word16BE (fromIntegral $ length as) <> foldr ((<>) . encode) mempty as

-- | A single attribute.
data Attribute = Unknown Utf8Ref ByteString

instance Show Attribute where
  show (Unknown u b) = "Unknown " ++ show u ++ " " ++ show b

instance Component Attribute where
  parser =
    Unknown <$> parser <*> do
      attributeSize <- anyWord32
      A.take (fromIntegral attributeSize)
  encode (Unknown u b) =
    encode u <> word32BE (fromIntegral $ BS.length b) <> byteString b

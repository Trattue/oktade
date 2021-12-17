-- |
-- Module: Data.Oktade.Classfile.Class.Attributes
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile attributes.
module Data.Oktade.Classfile.Class.Attributes
  ( -- * Attributes
    Attributes (..),

    -- ** Attribute Constant Pool Refs
    NSourceFile (..),

    -- ** Class Attributes
    Attribute (..),
  )
where

import Data.Attoparsec.ByteString (choice)
import Data.Attoparsec.ByteString.Lazy (count)
import qualified Data.Attoparsec.ByteString.Lazy as A (take)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteString, word16BE, word32BE)
import Data.IntMap ((!?))
import Data.Oktade.ByteParser (anyWord16, anyWord32)
import Data.Oktade.Classfile.Class.Parse (Parse (..), Unparse (..))
import Data.Oktade.Classfile.Metadata (Metadata (constantPool))
import Data.Oktade.Classfile.Metadata.ConstantPool
  ( ConstantPool (..),
    ConstantPoolEntry (Utf8),
    Utf8Ref (..),
  )
import qualified Data.Oktade.Parse as P (parser, unparser)
import Data.String (fromString)

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

--------------------------------------------------------------------------------
-- Attribute Names
--------------------------------------------------------------------------------

newtype NSourceFile = NSourceFile Utf8Ref
  deriving (Show)

instance Parse NSourceFile where
  parser m = do
    idx <- anyWord16
    let (Just (Utf8 _ bs)) = entries (constantPool m) !? fromIntegral idx
    if bs == fromString "SourceFile"
      then return $ NSourceFile $ Utf8Ref idx
      else fail ""

instance Unparse NSourceFile where
  unparser _ (NSourceFile u) = P.unparser u

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

-- | A single attribute.
data Attribute
  = -- | Name of the original source file.
    SourceFile NSourceFile Utf8Ref
  | Unknown Utf8Ref ByteString
  deriving (Show)

instance Parse Attribute where
  parser m =
    let parsers =
          [ parserSourceFile,
            parserUnknown
          ]
     in choice parsers
    where
      parserSourceFile =
        SourceFile <$> parser m <*> do
          _ <- anyWord32 -- attribute data size
          P.parser
      parserUnknown =
        Unknown <$> P.parser <*> do
          attributeSize <- anyWord32
          A.take (fromIntegral attributeSize)

instance Unparse Attribute where
  unparser m (SourceFile n u) = unparser m n <> word32BE 2 <> P.unparser u
  unparser _ (Unknown u b) =
    P.unparser u <> word32BE (fromIntegral $ BS.length b) <> byteString b

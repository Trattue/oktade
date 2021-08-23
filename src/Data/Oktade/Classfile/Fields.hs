-- |
-- Module      : Data.Oktade.Classfile.Fields
-- License     : Apache-2.0
--
-- This module contains type definitions and parsers for the classfile
-- fields.
module Data.Oktade.Classfile.Fields
  ( -- * Fields
    Fields (..),
    Field (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (count)
import Data.ByteString.Builder (word16BE)
import Data.Oktade.Classfile.Attributes (Attributes)
import Data.Oktade.Classfile.ConstantPool (Utf8Ref)
import Data.Oktade.Classfile.Fields.AccessFlags (AccessFlags)
import Data.Oktade.Internal.Bytecode (Bytecode (..))
import Data.Oktade.Internal.Parser (anyWord16)

--------------------------------------------------------------------------------
-- Fields
--------------------------------------------------------------------------------

-- | Represents the list of fields a classfile has.
newtype Fields = Fields [Field]

instance Show Fields where
  show (Fields []) = "Fields: -"
  show (Fields fs) =
    "Fields:\n"
      ++ init (unlines $ (init . unlines . (("  " ++) <$>) . lines) . show <$> fs)

instance Bytecode Fields where
  parser =
    Fields <$> do
      fieldCount <- anyWord16
      count (fromIntegral fieldCount) parser
  encode (Fields fs) =
    word16BE (fromIntegral $ length fs) <> foldr ((<>) . encode) mempty fs

-- | A single field.
data Field = Field AccessFlags Utf8Ref Utf8Ref Attributes

instance Show Field where
  show (Field a u u' as) =
    "Field:\n"
      ++ init (unlines $ ("  " ++) <$> lines (show a))
      ++ "\n  Name: "
      ++ show u
      ++ "\n  Descriptor: "
      ++ show u'
      ++ "\n"
      ++ init (unlines $ ("  " ++) <$> lines (show as))

instance Bytecode Field where
  parser = Field <$> parser <*> parser <*> parser <*> parser
  encode (Field a u u' as) = encode a <> encode u <> encode u' <> encode as

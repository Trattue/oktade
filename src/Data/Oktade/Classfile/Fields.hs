-- |
-- Module      : Data.Oktade.Classfile.Fields
-- License     : Apache-2.0
--
-- Type definitions for the classfile fields.
module Data.Oktade.Classfile.Fields
  ( -- * Fields
    Fields (..),
    Field (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (count)
import Data.ByteString.Builder (word16BE)
import Data.Oktade.ByteParser (anyWord16)
import Data.Oktade.Classfile.Attributes (Attributes)
import Data.Oktade.Classfile.ConstantPool (Utf8Ref)
import Data.Oktade.Classfile.Fields.AccessFlags (AccessFlags)
import Data.Oktade.Component (Component (..))

--------------------------------------------------------------------------------
-- Fields
--------------------------------------------------------------------------------

-- | Represents the list of fields a classfile has.
--
-- Read the JVM spec for more information:
-- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.5
newtype Fields = Fields [Field]

instance Show Fields where
  show (Fields []) = "Fields: -"
  show (Fields fs) =
    "Fields:\n"
      ++ init (unlines $ (init . unlines . (("  " ++) <$>) . lines) . show <$> fs)

instance Component Fields where
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

instance Component Field where
  parser = Field <$> parser <*> parser <*> parser <*> parser
  encode (Field a u u' as) = encode a <> encode u <> encode u' <> encode as

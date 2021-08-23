-- |
-- Module      : Data.Oktade.Classfile.Methods
-- License     : Apache-2.0
--
-- This module contains type definitions and parsers for the classfile methods.
module Data.Oktade.Classfile.Methods
  ( -- * Methods
    Methods (..),
    Method (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (count)
import Data.ByteString.Builder (word16BE)
import Data.Oktade.Classfile.Attributes (Attributes)
import Data.Oktade.Classfile.ConstantPool (Utf8Ref)
import Data.Oktade.Classfile.Methods.AccessFlags (AccessFlags)
import Data.Oktade.Internal.Bytecode (Bytecode (..))
import Data.Oktade.Internal.Parser (anyWord16)

--------------------------------------------------------------------------------
-- Methods
--------------------------------------------------------------------------------

-- | Represents the list of methods a classfile has.
newtype Methods = Methods [Method]

instance Show Methods where
  show (Methods []) = "Methods: -"
  show (Methods ms) = "Methods:\n" ++ init (unlines $ (init . unlines . (("  " ++) <$>) . lines) . show <$> ms)

instance Bytecode Methods where
  parser =
    Methods <$> do
      methodCount <- anyWord16
      count (fromIntegral methodCount) parser
  encode (Methods ms) =
    word16BE (fromIntegral $ length ms) <> foldr ((<>) . encode) mempty ms

-- | A single method.
data Method = Method AccessFlags Utf8Ref Utf8Ref Attributes

instance Show Method where
  show (Method a u u' as) =
    "Method:\n"
      ++ init (unlines $ ("  " ++) <$> lines (show a))
      ++ "\n  Name: "
      ++ show u
      ++ "\n  Descriptor: "
      ++ show u'
      ++ "\n"
      ++ init (unlines $ ("  " ++) <$> lines (show as))

instance Bytecode Method where
  parser = Method <$> parser <*> parser <*> parser <*> parser
  encode (Method a u u' as) = encode a <> encode u <> encode u' <> encode as

-- |
-- Module      : Data.Oktade.Classfile.Interfaces
-- License     : Apache-2.0
--
-- Type definitions for the classfile interfaces.
module Data.Oktade.Classfile.Interfaces
  ( -- * Interfaces
    Interfaces (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (count)
import Data.ByteString.Builder (word16BE)
import Data.Oktade.Classfile.ConstantPool (ClassRef)
import Data.Oktade.Internal.Bytecode (Bytecode (..))
import Data.Oktade.Internal.Parser (anyWord16)

--------------------------------------------------------------------------------
-- Interfaces
--------------------------------------------------------------------------------

-- | Represents the classfile interfaces (a list of 'ClassRef's, interfaces on
-- the constant pool)
--
-- Read the JVM spec for more information:
-- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.1
newtype Interfaces = Interfaces [ClassRef]

instance Show Interfaces where
  show (Interfaces []) = "Interfaces: -"
  show (Interfaces cs) =
    "Interfaces:\n" ++ init (unlines $ ("  " ++) . show <$> cs)

instance Bytecode Interfaces where
  parser =
    Interfaces <$> do
      interfaceCount <- anyWord16
      count (fromIntegral interfaceCount) parser
  encode (Interfaces cs) =
    word16BE (fromIntegral $ length cs) <> foldr ((<>) . encode) mempty cs

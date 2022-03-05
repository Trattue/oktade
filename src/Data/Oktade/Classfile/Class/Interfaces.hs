-- |
-- Module: Data.Oktade.Classfile.Class.Interfaces
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile interfaces.
module Data.Oktade.Classfile.Class.Interfaces
  ( -- * Interfaces
    Interfaces (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (count)
import Data.ByteString.Builder (word16BE)
import Data.Oktade.ByteParser (anyWord16)
import Data.Oktade.Classfile.Class.Parse (Parse (..), Unparse (..))
import Data.Oktade.Classfile.Metadata.ConstantPool (ClassRef)
import qualified Data.Oktade.Parse as P (parser, unparser)

--------------------------------------------------------------------------------
-- Interfaces
--------------------------------------------------------------------------------

-- | Classfile interfaces (a list of 'ClassRef's, interfaces on the constant
-- pool)
--
-- Read the JVM specification for more information:
-- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.1
newtype Interfaces = Interfaces [ClassRef]
  deriving (Show)

instance Parse Interfaces where
  parser _ = Interfaces <$> (anyWord16 >>= flip count P.parser . fromIntegral)

instance Unparse Interfaces where
  unparser _ (Interfaces cs) =
    word16BE (fromIntegral $ length cs) <> foldr ((<>) . P.unparser) mempty cs

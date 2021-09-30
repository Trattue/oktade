-- |
-- Module: Data.Oktade.Classfile.Metadata.MagicNumber
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile magic number.
module Data.Oktade.Classfile.Metadata.MagicNumber
  ( -- * MagicNumber
    MagicNumber (..),
  )
where

import Data.ByteString.Builder (word32BE)
import Data.Oktade.ByteConstant (Word32Constant (..))
import Data.Oktade.ByteParser (word32)
import Data.Oktade.Parse (Parse (..), Unparse (..))

--------------------------------------------------------------------------------
-- Magic Number
--------------------------------------------------------------------------------

-- | The classfile magic number, 0xCAFEBABE.
--
-- More about the magic number can be learned in the JVM specification:
-- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.1
data MagicNumber = Cafebabe
  deriving (Show, Eq)

instance Word32Constant MagicNumber where
  value32 Cafebabe = 0xCAFEBABE

instance Parse MagicNumber where
  parser = Cafebabe <$ word32 (value32 Cafebabe)

instance Unparse MagicNumber where
  unparser = word32BE . value32

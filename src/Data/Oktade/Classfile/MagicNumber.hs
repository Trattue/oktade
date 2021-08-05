-- |
-- Module      : Data.Oktade.Classfile.MagicNumber
-- License     : Apache-2.0
--
-- This module contains type definitions regarding the classfile magic number.
module Data.Oktade.Classfile.MagicNumber
  ( -- * Magic Number
    MagicNumber (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (string)
import Data.ByteString.Builder (byteString)
import Data.Oktade.ByteConstant (ByteStringConstant (..))
import Data.Oktade.Internal.Bytecode (Bytecode (..))

--------------------------------------------------------------------------------
-- Magic Number
--------------------------------------------------------------------------------

-- | The classfile magic number, 0xCAFEBABE.
--
-- JVM spec:
-- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.1
data MagicNumber = Cafebabe
  deriving (Eq, Show)

instance ByteStringConstant MagicNumber where
  rawValue Cafebabe = [0xCA, 0xFE, 0xBA, 0xBE]

instance Bytecode MagicNumber where
  parser = Cafebabe <$ string (value Cafebabe)
  encode m = byteString $ value m

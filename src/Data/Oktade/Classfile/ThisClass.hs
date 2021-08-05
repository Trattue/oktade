-- |
-- Module      : Data.Oktade.Classfile.ThisClass
-- License     : Apache-2.0
--
-- This module contains type definitions and parsers for the classfile this
-- class.
module Data.Oktade.Classfile.ThisClass
  ( -- * This Class
    ThisClass (..),
  )
where

import Data.ByteString.Builder (word16BE)
import Data.Oktade.Classfile.ConstantPool (ClassRef (ClassRef))
import Data.Oktade.Internal.Bytecode (Bytecode (..))
import Data.Oktade.Internal.Parser (anyWord16)

--------------------------------------------------------------------------------
-- This Class
--------------------------------------------------------------------------------

-- | Reference to the current class in the constant pool.
newtype ThisClass = ThisClass ClassRef
  deriving (Show)

instance Bytecode ThisClass where
  parser = ThisClass . ClassRef <$> anyWord16
  encode (ThisClass (ClassRef c)) = word16BE c
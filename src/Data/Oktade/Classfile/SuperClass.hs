-- |
-- Module      : Data.Oktade.Classfile.SuperClass
-- License     : Apache-2.0
--
-- This module contains type definitions and parsers for the classfile super
-- class.
module Data.Oktade.Classfile.SuperClass
  ( -- * Super Class
    SuperClass (..),
  )
where

import Data.ByteString.Builder (word16BE)
import Data.Oktade.Classfile.ConstantPool (ClassRef (ClassRef))
import Data.Oktade.Internal.Bytecode (Bytecode (..))
import Data.Oktade.Internal.Parser (anyWord16)

--------------------------------------------------------------------------------
-- Super Class
--------------------------------------------------------------------------------

-- | Reference to the super class in the constant pool or 'Object', if no super
-- class is defined.
data SuperClass = Object | SuperClass ClassRef
  deriving (Show)

instance Bytecode SuperClass where
  parser = do
    super <- anyWord16
    return $ if super == 0 then Object else SuperClass $ ClassRef super
  encode Object = word16BE 0
  encode (SuperClass c) = encode c

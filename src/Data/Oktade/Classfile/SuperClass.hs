-- |
-- Module      : Data.Oktade.Classfile.SuperClass
-- License     : Apache-2.0
--
-- Type definitions for the classfile super class.
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

instance Show SuperClass where
  show Object = "Super class: Object"
  show (SuperClass c) = "Super class: " ++ show c

instance Bytecode SuperClass where
  parser = do
    super <- anyWord16
    return $ if super == 0 then Object else SuperClass $ ClassRef super
  encode Object = word16BE 0
  encode (SuperClass c) = encode c

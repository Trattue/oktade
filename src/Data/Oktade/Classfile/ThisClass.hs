-- |
-- Module      : Data.Oktade.Classfile.ThisClass
-- License     : Apache-2.0
--
-- Type definitions for the classfile this class.
module Data.Oktade.Classfile.ThisClass
  ( -- * This Class
    ThisClass (..),
  )
where

import Data.ByteString.Builder (word16BE)
import Data.Oktade.ByteParser (anyWord16)
import Data.Oktade.Classfile.ConstantPool (ClassRef (ClassRef))
import Data.Oktade.Component (Component (..))

--------------------------------------------------------------------------------
-- This Class
--------------------------------------------------------------------------------

-- | Reference to the current class in the constant pool.
newtype ThisClass = ThisClass ClassRef

instance Show ThisClass where
  show (ThisClass c) = "This class: " ++ show c

instance Component ThisClass where
  parser = ThisClass . ClassRef <$> anyWord16
  encode (ThisClass (ClassRef c)) = word16BE c

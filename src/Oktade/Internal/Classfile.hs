-- |
-- Module: Oktade.Internal.Classfile
-- License: Apache-2.0
module Oktade.Internal.Classfile where

import Oktade.Internal.Classfile.AccessFlags (AccessFlags)
import Oktade.Internal.Classfile.ConstantPool (ConstantPool)
import Oktade.Internal.Classfile.Version (Version)

data Classfile = Classfile
  { version :: Version,
    constantPool :: ConstantPool,
    accessFlags :: AccessFlags,
    thisClass :: Int,
    superClass :: Int,
    interfaces :: Int,
    fields :: Int,
    methods :: Int,
    attributes :: Int
  }

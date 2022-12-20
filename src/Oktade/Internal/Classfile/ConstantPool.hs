module Oktade.Internal.Classfile.ConstantPool where

import Data.IntMap (IntMap)

newtype ConstantPool = ConstantPool (IntMap ConstantPoolEntry)

data ConstantPoolEntry
  = Utf8
  | Integer
  | Float
  | Long
  | Double
  | Class
  | String
  | Fieldref
  | Methodref
  | InterfaceMethodref
  | NameAndType
  | MethodHandle
  | MethodType
  | Dynamic
  | InvokeDynamic
  | Module
  | Package

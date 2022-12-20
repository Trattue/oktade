module Oktade.Internal.Classfile.AccessFlags where

newtype AccessFlags = AccessFlags [AccessFlag]

data AccessFlag
  = Public
  | Final
  | Super
  | Interface
  | Abstract
  | Synthetic
  | Annotation
  | Enum
  | Module

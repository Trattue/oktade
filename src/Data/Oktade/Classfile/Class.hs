-- |
-- Module: Data.Oktade.Classfile.Class
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the class data of classfiles.
module Data.Oktade.Classfile.Class where

import Data.Oktade.Classfile.Class.AccessFlags (AccessFlags)
import Data.Oktade.Classfile.Class.Attributes (Attributes)
import Data.Oktade.Classfile.Class.Fields (Fields)
import Data.Oktade.Classfile.Class.Interfaces (Interfaces)
import Data.Oktade.Classfile.Class.Methods (Methods)
import Data.Oktade.Classfile.Class.Parse (Parse (..))
import Data.Oktade.Classfile.Class.SuperClass (SuperClass)
import Data.Oktade.Classfile.Class.ThisClass (ThisClass)

data Class = Class
  { accessFlags :: AccessFlags,
    this :: ThisClass,
    super :: SuperClass,
    interfaces :: Interfaces,
    fields :: Fields,
    methods :: Methods,
    attributes :: Attributes
  }
  deriving (Show)

instance Parse Class where
  parser m =
    Class
      <$> parser m
      <*> parser m
      <*> parser m
      <*> parser m
      <*> parser m
      <*> parser m
      <*> parser m

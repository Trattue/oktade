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
import Data.Oktade.Classfile.Class.Parse (Parse (..), Unparse (..))
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

instance Unparse Class where
  unparser m c =
    unparser m (accessFlags c)
      <> unparser m (this c)
      <> unparser m (super c)
      <> unparser m (interfaces c)
      <> unparser m (fields c)
      <> unparser m (methods c)
      <> unparser m (attributes c)

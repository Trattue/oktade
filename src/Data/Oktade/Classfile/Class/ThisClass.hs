-- |
-- Module: Data.Oktade.Classfile.Class.ThisClass
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile this class.
module Data.Oktade.Classfile.Class.ThisClass
  ( -- * This Class
    ThisClass (..),
  )
where

import Data.Oktade.Classfile.Class.Parse (Parse (..), Unparse (..))
import Data.Oktade.Classfile.Metadata.ConstantPool (ClassRef)
import qualified Data.Oktade.Parse as P (parser, unparser)

--------------------------------------------------------------------------------
-- This Class
--------------------------------------------------------------------------------

-- | Reference to the current class in the constant pool. The JVM specification
-- requires the reference to show to a Class type entry; neither this type nor
-- the corresponding 'Parse' implementation enforce this.
--
-- More about the this class can be learned in the JVM specification:
-- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.1
newtype ThisClass = ThisClass ClassRef
  deriving (Show)

instance Parse ThisClass where
  parser _ = ThisClass <$> P.parser

instance Unparse ThisClass where
  unparser _ (ThisClass c) = P.unparser c

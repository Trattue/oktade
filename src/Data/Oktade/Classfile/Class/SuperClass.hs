-- |
-- Module: Data.Oktade.Classfile.Class.SuperClass
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile super class.
module Data.Oktade.Classfile.Class.SuperClass
  ( -- * Super Class
    SuperClass (..),
  )
where

import Data.ByteString.Builder (word16BE)
import Data.Oktade.Classfile.Class.Parse (Parse (..), Unparse (..))
import Data.Oktade.Classfile.Metadata.ConstantPool (ClassRef (ClassRef))
import qualified Data.Oktade.Parse as P (parser, unparser)

--------------------------------------------------------------------------------
-- Super Class
--------------------------------------------------------------------------------

-- | Reference to the super class in the constant pool or 'Object', if no super
-- class is defined. The JVM specification requires the reference to show to a
-- Class type entry and has some restirctions on access flags of the
-- corresponding class; neither this type nor the corresponding 'Parse'
-- implementation enforce this.
--
-- More about the super class can be learned in the JVM specification:
-- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.1
data SuperClass = Object | SuperClass ClassRef
  deriving (Show)

instance Parse SuperClass where
  parser _ = do
    c@(ClassRef s) <- P.parser
    return $ if s == 0 then Object else SuperClass c

instance Unparse SuperClass where
  unparser _ Object = word16BE 0
  unparser _ (SuperClass c) = P.unparser c

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
-- class is defined.
data SuperClass = Object | SuperClass ClassRef
  deriving (Show)

instance Parse SuperClass where
  parser _ = do
    c@(ClassRef s) <- P.parser
    return $ if s == 0 then Object else SuperClass c

instance Unparse SuperClass where
  unparser _ Object = word16BE 0
  unparser _ (SuperClass c) = P.unparser c

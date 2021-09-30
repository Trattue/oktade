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

import Data.ByteString.Builder (word16BE)
import Data.Oktade.ByteParser (anyWord16)
import Data.Oktade.Classfile.Class.Parse (Parse (..), Unparse (..))
import Data.Oktade.Classfile.Metadata.ConstantPool (ClassRef)
import qualified Data.Oktade.Parse as P (parser, unparser)

--------------------------------------------------------------------------------
-- This Class
--------------------------------------------------------------------------------

-- | Reference to the current class in the constant pool.
newtype ThisClass = ThisClass ClassRef
  deriving (Show)

instance Parse ThisClass where
  parser _ = ThisClass <$> P.parser

instance Unparse ThisClass where
  unparser _ (ThisClass c) = P.unparser c

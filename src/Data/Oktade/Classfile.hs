-- |
-- Module      : Data.Oktade.Classfile
-- License     : Apache-2.0
--
-- This module contains type definitions regarding the general classfile.
module Data.Oktade.Classfile
  ( -- * Classfile
    Classfile (..),
  )
where

import Data.Oktade.Classfile.MagicNumber (MagicNumber)
import Data.Oktade.Classfile.Version (Version)
import Data.Oktade.Internal.Bytecode (Bytecode (..))

--------------------------------------------------------------------------------
-- Classfile
--------------------------------------------------------------------------------

-- | Data structure for the whole classfile.
--
-- JVM spec:
-- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.1
data Classfile = Classfile
  { magic :: MagicNumber,
    version :: Version
  } -- TODO
  deriving (Show)

instance Bytecode Classfile where
  parser = Classfile <$> parser <*> parser
  encode c = encode (magic c) <> encode (version c)

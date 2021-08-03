-- |
-- Module      : Data.Oktade.Classfile.Version
-- License     : Apache-2.0
--
-- This module contains type definitions regarding the classfile magic number.
module Data.Oktade.Classfile.Version
  ( -- * Version
    Version (..),
    Major,
    Minor,
  )
where

import Data.ByteString.Builder (word16Dec)
import Data.Oktade.Internal.Bytecode (Bytecode (..))
import Data.Oktade.Parser (anyWord16)
import Data.Word (Word16)

--------------------------------------------------------------------------------
-- Version
--------------------------------------------------------------------------------

-- | The classfile version.
-- Note: In the actual classfile, the minor version is before the major version.
--
-- JVM spec:
-- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.1
data Version = Version Major Minor
  deriving (Show)

-- | The classfile major version.
type Major = Word16

-- | The classfile minor version. Major versions above 55 set some restrictions
-- on the minor version, see the JVM spec.
type Minor = Word16

instance Bytecode Version where
  parser = do
    minor <- anyWord16
    major <- anyWord16
    return $ Version major minor
  encode (Version major minor) = word16Dec minor <> word16Dec major

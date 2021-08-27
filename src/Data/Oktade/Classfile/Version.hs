-- |
-- Module      : Data.Oktade.Classfile.Version
-- License     : Apache-2.0
--
-- Type definitions for the classfile version.
module Data.Oktade.Classfile.Version
  ( -- * Version
    Version (..),
    Major,
    Minor,
  )
where

import Data.ByteString.Builder (word16BE)
import Data.Oktade.ByteParser (anyWord16)
import Data.Oktade.Component (Component (..))
import Data.Word (Word16)

--------------------------------------------------------------------------------
-- Version
--------------------------------------------------------------------------------

-- | The classfile version.
--
-- Note: In the actual classfile, the minor version is before the major version.
--
-- More about the classfile version can be learned in the JVM spec:
-- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.1
data Version = Version Major Minor

instance Show Version where
  show (Version major minor) = "Version: " ++ show major ++ "." ++ show minor

-- | The classfile major version.
type Major = Word16

-- | The classfile minor version. Major versions above 55 set some restrictions
-- on the minor version; read the JVM spec for more information.
type Minor = Word16

instance Component Version where
  parser = do
    minor <- anyWord16
    major <- anyWord16
    return $ Version major minor
  encode (Version major minor) = word16BE minor <> word16BE major

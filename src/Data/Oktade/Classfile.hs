-- |
-- Module      : Data.Oktade.Classfile
-- License     : Apache-2.0
--
-- This module contains type definitions regarding the general classfile.
module Data.Oktade.Classfile
  ( parseClassfile,
    encodeClassfile,

    -- * Classfile
    Classfile (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (Parser, Result, parse)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (ByteString)
import Data.Oktade.Classfile.ConstantPool (ConstantPool)
import Data.Oktade.Classfile.MagicNumber (MagicNumber)
import Data.Oktade.Classfile.Version (Version)
import Data.Oktade.Internal.Bytecode (Bytecode (..))

-- Temp
parseClassfile :: ByteString -> Result Classfile
parseClassfile = parse (parser :: Parser Classfile)

-- Temp
encodeClassfile :: Classfile -> ByteString
encodeClassfile c = toLazyByteString $ encode c

--------------------------------------------------------------------------------
-- Classfile
--------------------------------------------------------------------------------

-- | Data structure for the whole classfile.
--
-- JVM spec:
-- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.1
data Classfile = Classfile
  { magic :: MagicNumber,
    version :: Version,
    constantPool :: ConstantPool
  } -- TODO
  deriving (Show)

instance Bytecode Classfile where
  parser = Classfile <$> parser <*> parser <*> parser
  encode c = encode (magic c) <> encode (version c) <> encode (constantPool c)

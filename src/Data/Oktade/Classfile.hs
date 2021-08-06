-- |
-- Module      : Data.Oktade.Classfile
-- License     : Apache-2.0
--
-- This module contains type definitions and parsers for the general classfile.
module Data.Oktade.Classfile
  ( -- * Parsing and Encoding
    parseClassfile,
    encodeClassfile,

    -- * Classfile
    Classfile (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (Parser, Result, parse)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (ByteString)
import Data.Oktade.Classfile.AccessFlags (AccessFlags)
import Data.Oktade.Classfile.ConstantPool (ConstantPool)
import Data.Oktade.Classfile.MagicNumber (MagicNumber)
import Data.Oktade.Classfile.SuperClass (SuperClass)
import Data.Oktade.Classfile.ThisClass (ThisClass)
import Data.Oktade.Classfile.Version (Version)
import Data.Oktade.Internal.Bytecode (Bytecode (..))

--------------------------------------------------------------------------------
-- Parsing and Encoding
--------------------------------------------------------------------------------

-- Parses a 'Classfile' from a 'ByteString'.
parseClassfile :: ByteString -> Result Classfile
parseClassfile = parse (parser :: Parser Classfile)

-- Encodes a 'Classfile' to a 'ByteString'
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
    constantPool :: ConstantPool,
    accessFlags :: AccessFlags,
    thisClass :: ThisClass,
    superClass :: SuperClass
  } -- TODO

instance Show Classfile where
  show c =
    init $
      unlines
        [ show (magic c),
          show (version c),
          show (constantPool c),
          show (accessFlags c),
          show (thisClass c),
          show (superClass c)
        ]

instance Bytecode Classfile where
  parser =
    Classfile <$> parser <*> parser <*> parser <*> parser <*> parser <*> parser
  encode c =
    encode (magic c)
      <> encode (version c)
      <> encode (constantPool c)
      <> encode (accessFlags c)
      <> encode (thisClass c)
      <> encode (superClass c)

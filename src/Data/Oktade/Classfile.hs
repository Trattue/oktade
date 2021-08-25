-- |
-- Module      : Data.Oktade.Classfile
-- License     : Apache-2.0
--
-- Type definitions and parsers/encoders for the general classfile.
module Data.Oktade.Classfile
  ( -- * Parsing and Encoding
    -- $parsing_and_encoding
    parseClassfile,
    encodeClassfile,

    -- * Classfile
    Classfile (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (Parser, Result, parse)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (ByteString)
import Data.List (intercalate)
import Data.Oktade.Classfile.AccessFlags (AccessFlags)
import Data.Oktade.Classfile.Attributes (Attributes)
import Data.Oktade.Classfile.ConstantPool (ConstantPool)
import Data.Oktade.Classfile.Fields (Fields)
import Data.Oktade.Classfile.Interfaces (Interfaces)
import Data.Oktade.Classfile.MagicNumber (MagicNumber)
import Data.Oktade.Classfile.Methods (Methods)
import Data.Oktade.Classfile.SuperClass (SuperClass)
import Data.Oktade.Classfile.ThisClass (ThisClass)
import Data.Oktade.Classfile.Version (Version)
import Data.Oktade.Internal.Bytecode (Bytecode (..))

--------------------------------------------------------------------------------
-- Parsing and Encoding
--------------------------------------------------------------------------------

-- $parsing_and_encoding
--
-- Behind the scenes, oktade uses the attoparsec parser combinator library for
-- parsing classfiles. Oktade, however, hides most implementation details of the
-- actual parsing and encoding for the library user. This section defines a top
-- level interface for using oktade's parsers and encoders.
--
-- Oktade is able to translate a given 'ByteString', defined by the bytestring
-- library, to a data structure representing a classfile ('Classfile') and
-- encode this data structure back to a 'ByteString'. Both parsing and encoding
-- happen in a lazy manner. To indicate parser failure, oktade simply uses
-- attoparsec's 'Result' type.
--
-- Example usage for reading a classfile and printing its parsed content:
--
-- >import Data.Attoparsec.ByteString.Lazy (Result (Done, Fail))
-- >import Data.ByteString.Lazy as BS (readFile)
-- >import Data.Oktade.Classfile (encodeClassfile, parseClassfile)
-- >
-- >printClassfile :: FilePath -> IO ()
-- >printClassfile p = do
-- >  classfile <- BS.readFile p
-- >  case parseClassfile classfile of
-- >    Fail _ _ e -> putStrLn $ show p ++ " could not be parsed! Error: " ++ e
-- >    Done _ r ->
-- >      putStrLn $ show p ++ " successfully parsed! Result:\n" ++ show r

-- | Parses a 'Classfile' from a lazy 'ByteString'. Returns a 'Result' (defined
-- in the attoparsec library) to indicate successful parsing ('Done') or failure
-- ('Fail'). See the attoparsec documentation for more information about the
-- 'Result' type.
--
-- Note that any data after the actual classfile will be ignored by the parser
-- and returned as remaining input in the the 'Result' type.
parseClassfile :: ByteString -> Result Classfile
parseClassfile = parse (parser :: Parser Classfile)

-- | Encodes a 'Classfile' to a lazy 'ByteString'. The resulting 'ByteString'
-- will exactly match the consumed input 'ByteString' (assuming the 'Classfile'
-- wasn't modified):
--
-- >>> let (Done _ result) = parseClassfile classfile
-- >>> encodeClassfile result `isPrefixOf` classfile
-- True
--
-- Note: Classfiles are encoded in Big Endian.
encodeClassfile :: Classfile -> ByteString
encodeClassfile c = toLazyByteString $ encode c

--------------------------------------------------------------------------------
-- Classfile
--------------------------------------------------------------------------------

-- | Data structure for the whole classfile. The structure of this type follows
-- closely the overall structure of the classfile format.
--
-- The structure of the classfile format can be understood reading the JVM spec:
-- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.1
data Classfile = Classfile
  { magic :: MagicNumber,
    version :: Version,
    constantPool :: ConstantPool,
    accessFlags :: AccessFlags,
    thisClass :: ThisClass,
    superClass :: SuperClass,
    interfaces :: Interfaces,
    fields :: Fields,
    methods :: Methods,
    attributes :: Attributes
  }

instance Show Classfile where
  show c =
    let components =
          [ show (magic c),
            show (version c),
            show (constantPool c),
            show (accessFlags c),
            show (thisClass c),
            show (superClass c),
            show (interfaces c),
            show (fields c),
            show (methods c),
            show (attributes c)
          ]
     in intercalate "\n" components

instance Bytecode Classfile where
  parser =
    Classfile
      <$> parser
      <*> parser
      <*> parser
      <*> parser
      <*> parser
      <*> parser
      <*> parser
      <*> parser
      <*> parser
      <*> parser
  encode c =
    encode (magic c)
      <> encode (version c)
      <> encode (constantPool c)
      <> encode (accessFlags c)
      <> encode (thisClass c)
      <> encode (superClass c)
      <> encode (interfaces c)
      <> encode (fields c)
      <> encode (methods c)
      <> encode (attributes c)

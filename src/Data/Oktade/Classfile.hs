-- |
-- Module: Data.Oktade.Classfile
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing classfiles.
module Data.Oktade.Classfile
  ( -- * Parsing and Unparsing
    -- $parsing_and_unparsing
    parseClassfile,
    unparseClassfile,

    -- * Classfile
    Classfile (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (Result, parse)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (ByteString)
import Data.Oktade.Classfile.Class (Class)
import qualified Data.Oktade.Classfile.Class.Parse as P
import Data.Oktade.Classfile.Metadata (Metadata)
import Data.Oktade.Parse (Parse, Unparse, parser, unparser)

--------------------------------------------------------------------------------
-- Parsing and Unparsing
--------------------------------------------------------------------------------

-- $parsing_and_unparsing
--
-- Behind the scenes, oktade uses the attoparsec parser combinator library for
-- parsing classfiles. This section defines a top level interface for using
-- oktades parsers and unparsers.
--
-- Oktade is able to translate a given 'ByteString', defined by the bytestring
-- library, to a data structure representing a classfile ('Classfile') and
-- unparse this data structure back to a 'ByteString'. Both parsing and
-- unparsing happen in a lazy manner. To indicate parser failure, oktade simply
-- uses attoparsec's 'Result' type.
--
-- Example for reading a classfile and printing its parsed content:
--
-- >import Data.Attoparsec.ByteString.Lazy (Result (Done, Fail))
-- >import Data.ByteString.Lazy as BS (readFile)
-- >import Data.Oktade.Classfile (parseClassfile, unparseClassfile)
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
parseClassfile = parse parser

-- | Unparses a 'Classfile' to a lazy 'ByteString'. The resulting 'ByteString'
-- will exactly match the consumed input 'ByteString' (assuming the 'Classfile'
-- wasn't modified):
--
-- >>> let (Done _ result) = parseClassfile classfile
-- >>> unparseClassfile result `isPrefixOf` classfile
-- True
--
-- Note: Classfiles are encoded in Big Endian.
unparseClassfile :: Classfile -> ByteString
unparseClassfile = toLazyByteString . unparser

--------------------------------------------------------------------------------
-- Classfile
--------------------------------------------------------------------------------

-- | Top level classfile data structure. In contrary to the structure described
-- in the JVM specification
-- (https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.1),
-- we do have another layer of abstraction in the classfile for easier parsing
-- by seperating metadata and the actual class data.
data Classfile = Classfile
  { -- | Abstraction for the first three metadata parts of the classfile:
    -- magic number, version and constant pool.
    metadata :: Metadata,
    -- | Abstraction for the class information parts of the classfile
    -- (everything except for the 'Metadata' parts).
    clazz :: Class
  }
  deriving (Show)

instance Parse Classfile where
  parser = do
    m <- parser
    c <- P.parser m
    return $ Classfile m c

instance Unparse Classfile where
  unparser c = unparser (metadata c) <> P.unparser (metadata c) (clazz c)

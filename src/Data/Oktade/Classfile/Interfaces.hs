module Data.Oktade.Classfile.Interfaces where

import Data.Attoparsec.ByteString (count)
import Data.ByteString.Builder (word16BE)
import Data.Map (fold)
import Data.Oktade.Classfile.ConstantPool (ClassRef)
import Data.Oktade.Internal.Bytecode (Bytecode (..))
import Data.Oktade.Internal.Parser (anyWord16)

newtype Interfaces = Interfaces [ClassRef]

instance Show Interfaces where
  show (Interfaces []) = "Interfaces: -"
  show (Interfaces cs) = "Interfaces:\n" ++ unlines (show <$> cs)

instance Bytecode Interfaces where
  parser =
    Interfaces <$> do
      interfaceCount <- anyWord16
      count (fromIntegral interfaceCount) parser
  encode (Interfaces cs) =
    word16BE (fromIntegral $ length cs) <> foldr ((<>) . encode) mempty cs

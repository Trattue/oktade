-- |
-- Module      : Data.Oktade.Classfile.AccessFlags
-- License     : Apache-2.0
--
-- Type definitions for the classfile access flags.
module Data.Oktade.Classfile.AccessFlags
  ( -- * Access Flags
    AccessFlags (..),
    AccessFlag (..),
  )
where

import Data.Bits (Bits ((.&.), (.|.)))
import Data.ByteString.Builder (word16BE)
import Data.Oktade.ByteConstant (Word16Constant (..))
import Data.Oktade.Internal.Bytecode (Bytecode (..))
import Data.Oktade.Internal.Parser (anyWord16)

--------------------------------------------------------------------------------
-- Access Flags
--------------------------------------------------------------------------------

-- | Represents the list of 'AccessFlag's the classfiles class/interface has.
newtype AccessFlags = AccessFlags [AccessFlag]

instance Show AccessFlags where
  show (AccessFlags []) = "Access Flags: -"
  show (AccessFlags af) =
    "Access Flags:\n" ++ init (unlines $ ("  " ++) . show <$> af)

instance Bytecode AccessFlags where
  parser =
    let flags =
          [ Public,
            Final,
            Super,
            Interface,
            Abstract,
            Synthetic,
            Annotation,
            Enum,
            Module
          ]
     in AccessFlags <$> do
          mask <- anyWord16
          return $ foldr (prependIfPresent mask) [] flags
    where
      prependIfPresent m f acc
        | m .&. value16 f == value16 f = f : acc
        | otherwise = acc
  encode (AccessFlags as) = word16BE $ foldr ((.|.) . value16) 0 as

-- | Represents a single access flag.
data AccessFlag
  = -- | Declared public, may be accessed from outside its package.
    Public
  | -- | Declared final, no subclasses allowed.
    Final
  | -- | Treat superclass methods specially when invoked by the invokespecial
    -- instruction.
    Super
  | -- | Is an interface, not a class.
    Interface
  | -- | Declared abstract, must not be instantiated.
    Abstract
  | -- | Declared synthetic, not present in the source code.
    Synthetic
  | -- | Declared as an annotation interface.
    Annotation
  | -- | Declared as an enum class.
    Enum
  | -- | Is a module, not a class or interface.
    Module
  deriving (Show)

instance Word16Constant AccessFlag where
  value16 Public = 0x0001
  value16 Final = 0x0010
  value16 Super = 0x0020
  value16 Interface = 0x0200
  value16 Abstract = 0x0400
  value16 Synthetic = 0x1000
  value16 Annotation = 0x2000
  value16 Enum = 0x4000
  value16 Module = 0x8000

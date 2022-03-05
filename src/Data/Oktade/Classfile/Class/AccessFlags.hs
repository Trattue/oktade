-- |
-- Module: Data.Oktade.Classfile.Class.AccessFlags
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile class access
-- flags.
module Data.Oktade.Classfile.Class.AccessFlags
  ( -- * Access Flags
    AccessFlags (..),
    AccessFlag (..),
  )
where

import Data.Bits ((.&.), (.|.))
import Data.ByteString.Builder (word16BE)
import Data.Oktade.ByteConstant (Word16Constant, value16)
import Data.Oktade.ByteParser (anyWord16)
import Data.Oktade.Classfile.Class.Parse (Parse (..), Unparse (..))

--------------------------------------------------------------------------------
-- Access Flags
--------------------------------------------------------------------------------

-- | List of 'AccessFlag's the classfile's class/interface has. In the
-- classfile, this structure is stored as a bit map.
--
-- More about the access flags can be learned in the JVM specification:
-- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.1
newtype AccessFlags = AccessFlags [AccessFlag]
  deriving (Show)

instance Parse AccessFlags where
  parser _ =
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

instance Unparse AccessFlags where
  unparser _ (AccessFlags as) = word16BE $ foldr ((.|.) . value16) 0 as

-- | A single access flag.
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
  deriving (Show, Eq)

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

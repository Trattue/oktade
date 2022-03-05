-- |
-- Module: Data.Oktade.Classfile.Class.Fields.AccessFlags
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the access flags of classfile
-- fields.
module Data.Oktade.Classfile.Class.Fields.AccessFlags
  ( -- * Access Flags
    FieldAccessFlags (..),
    FieldAccessFlag (..),
  )
where

import Data.Bits ((.&.), (.|.))
import Data.ByteString.Builder (word16BE)
import Data.Oktade.ByteConstant (Word16Constant (..))
import Data.Oktade.ByteParser (anyWord16)
import Data.Oktade.Classfile.Class.Parse (Parse (..), Unparse (..))

--------------------------------------------------------------------------------
-- Access Flags
--------------------------------------------------------------------------------

-- | List of 'AccessFlag's a field has. In the classfile, this structure is
-- stored as a bit map.
--
-- More about the fields access flags can be learned in the JVM specification:
-- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.5
newtype FieldAccessFlags = FieldAccessFlags [FieldAccessFlag]
  deriving (Show)

instance Parse FieldAccessFlags where
  parser _ =
    let flags =
          [ Public,
            Private,
            Protected,
            Static,
            Final,
            Volatile,
            Transient,
            Synthetic,
            Enum
          ]
     in FieldAccessFlags <$> do
          mask <- anyWord16
          return $ foldr (prependIfPresent mask) [] flags
    where
      prependIfPresent m f acc
        | m .&. value16 f == value16 f = f : acc
        | otherwise = acc

instance Unparse FieldAccessFlags where
  unparser _ (FieldAccessFlags as) = word16BE $ foldr ((.|.) . value16) 0 as

-- | A single field access flag.
data FieldAccessFlag
  = -- | Declared public, may be accessed from outside its package.
    Public
  | -- | Declared private, accessible only within the defining class and other
    -- classes belonging to the same nest.
    Private
  | -- | Declared public, may be accessed within subclasses.
    Protected
  | -- | Declared static.
    Static
  | -- | Declared final, never directly assigned to after object construction.
    Final
  | -- | Declared volatile, cannot be cached.
    Volatile
  | -- | Declared transient, not written or read by a persistent object manager.
    Transient
  | -- | Declared synthetic, not present in the source code.
    Synthetic
  | -- | Declared as an element of an enum class.
    Enum
  deriving (Show)

instance Word16Constant FieldAccessFlag where
  value16 Public = 0x0001
  value16 Private = 0x0002
  value16 Protected = 0x0004
  value16 Static = 0x0008
  value16 Final = 0x0010
  value16 Volatile = 0x0040
  value16 Transient = 0x0080
  value16 Synthetic = 0x1000
  value16 Enum = 0x4000

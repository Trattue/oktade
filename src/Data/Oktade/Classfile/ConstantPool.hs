-- |
-- Module      : Data.Oktade.Classfile.ConstantPool
-- License     : Apache-2.0
--
-- This module contains type definitions regarding the classfile constant pool
-- and its entries.
module Data.Oktade.Classfile.ConstantPool
  ( -- * Constant Pool
    ConstantPool (..),

    -- ** Constant Pool Tags
    TClass (..),
    TFieldRef (..),
    TMethodRef (..),
    TInterfaceMethodRef (..),
    TString (..),
    TInteger (..),
    TFloat (..),
    TLong (..),
    TDouble (..),
    TNameAndType (..),
    TUtf8 (..),
    TMethodHandle (..),
    TDynamic (..),
    TInvokeDynamic (..),
    TModule (..),
    TPackage (..),

    -- ** Constant Pool Entries
    ConstantPoolEntry (..),

    -- ** References
  )
where

import Data.Attoparsec.ByteString (choice, word8)
import qualified Data.Attoparsec.ByteString as BS (take)
import Data.ByteString (ByteString, length)
import Data.ByteString.Builder (byteString, word16BE, word32BE, word64BE)
import qualified Data.ByteString.Builder as B (byteString, word8)
import Data.IntMap (IntMap, fromAscList, size)
import Data.Oktade.ByteConstant (ByteStringConstant, Word8Constant (..))
import Data.Oktade.Internal.Bytecode (Bytecode (..))
import Data.Oktade.Internal.Parser (anyWord16, anyWord32, anyWord64)
import Data.Word (Word16, Word32, Word64, Word8)

--------------------------------------------------------------------------------
-- Constant Pool
--------------------------------------------------------------------------------

-- | Represents the classfile constant pool which is a list of
-- 'ConstantPoolEntry's mapped to their indices.
--
-- JVM spec:
-- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4
newtype ConstantPool = ConstantPool (IntMap ConstantPoolEntry)
  deriving (Show)

instance Bytecode ConstantPool where
  parser = do
    entryCountPlusOne <- anyWord16
    let entryCount = fromIntegral entryCountPlusOne - 1
    entries <- countC entryCount 1 parser []
    return $ ConstantPool $ fromAscList entries
    where
      countC c n p acc
        | n > c = return acc
        | otherwise = do
          x <- p
          countC c (n + constantPoolSize x) p ((n, x) : acc)
  encode (ConstantPool m) =
    word16BE (fromIntegral (sizeC m + 1)) <> foldr ((<>) . encode) mempty m
    where
      sizeC m = foldr (\x -> (+ constantPoolSize x)) 0 m

--------------------------------------------------------------------------------
-- Constant Pool Tags
--------------------------------------------------------------------------------

-- | Tag for the 'Class' constant pool entry.
data TClass = TClass
  deriving (Show)

instance Word8Constant TClass where
  value8 TClass = 7

instance Bytecode TClass where
  parser = TClass <$ word8 (value8 TClass)
  encode t = B.word8 $ value8 t

-- | Tag for the 'FieldRef' constant pool entry.
data TFieldRef = TFieldRef
  deriving (Show)

instance Word8Constant TFieldRef where
  value8 TFieldRef = 9

instance Bytecode TFieldRef where
  parser = TFieldRef <$ word8 (value8 TFieldRef)
  encode t = B.word8 $ value8 t

-- | Tag for the 'MethodRef' constant pool entry.
data TMethodRef = TMethodRef
  deriving (Show)

instance Word8Constant TMethodRef where
  value8 TMethodRef = 10

instance Bytecode TMethodRef where
  parser = TMethodRef <$ word8 (value8 TMethodRef)
  encode t = B.word8 $ value8 t

-- | Tag for the 'InterfaceMethodRef' constant pool entry.
data TInterfaceMethodRef = TInterfaceMethodRef
  deriving (Show)

instance Word8Constant TInterfaceMethodRef where
  value8 TInterfaceMethodRef = 11

instance Bytecode TInterfaceMethodRef where
  parser = TInterfaceMethodRef <$ word8 (value8 TInterfaceMethodRef)
  encode t = B.word8 $ value8 t

-- | Tag for the 'String' constant pool entry.
data TString = TString
  deriving (Show)

instance Word8Constant TString where
  value8 TString = 8

instance Bytecode TString where
  parser = TString <$ word8 (value8 TString)
  encode t = B.word8 $ value8 t

-- | Tag for the 'Integer' constant pool entry.
data TInteger = TInteger
  deriving (Show)

instance Word8Constant TInteger where
  value8 TInteger = 3

instance Bytecode TInteger where
  parser = TInteger <$ word8 (value8 TInteger)
  encode t = B.word8 $ value8 t

-- | Tag for the 'Float' constant pool entry.
data TFloat = TFloat
  deriving (Show)

instance Word8Constant TFloat where
  value8 TFloat = 4

instance Bytecode TFloat where
  parser = TFloat <$ word8 (value8 TFloat)
  encode t = B.word8 $ value8 t

-- | Tag for the 'Long' constant pool entry.
data TLong = TLong
  deriving (Show)

instance Word8Constant TLong where
  value8 TLong = 5

instance Bytecode TLong where
  parser = TLong <$ word8 (value8 TLong)
  encode t = B.word8 $ value8 t

-- | Tag for the 'Double' constant pool entry.
data TDouble = TDouble
  deriving (Show)

instance Word8Constant TDouble where
  value8 TDouble = 6

instance Bytecode TDouble where
  parser = TDouble <$ word8 (value8 TDouble)
  encode t = B.word8 $ value8 t

-- | Tag for the 'NameAndType' constant pool entry.
data TNameAndType = TNameAndType
  deriving (Show)

instance Word8Constant TNameAndType where
  value8 TNameAndType = 12

instance Bytecode TNameAndType where
  parser = TNameAndType <$ word8 (value8 TNameAndType)
  encode t = B.word8 $ value8 t

-- | Tag for the 'Utf8' constant pool entry.
data TUtf8 = TUtf8
  deriving (Show)

instance Word8Constant TUtf8 where
  value8 TUtf8 = 1

instance Bytecode TUtf8 where
  parser = TUtf8 <$ word8 (value8 TUtf8)
  encode t = B.word8 $ value8 t

-- | Tag for the 'MethodHandle' constant pool entry.
data TMethodHandle = TMethodHandle
  deriving (Show)

instance Word8Constant TMethodHandle where
  value8 TMethodHandle = 15

instance Bytecode TMethodHandle where
  parser = TMethodHandle <$ word8 (value8 TMethodHandle)
  encode t = B.word8 $ value8 t

-- | Tag for the 'MethodType' constant pool entry.
data TMethodType = TMethodType
  deriving (Show)

instance Word8Constant TMethodType where
  value8 TMethodType = 16

instance Bytecode TMethodType where
  parser = TMethodType <$ word8 (value8 TMethodType)
  encode t = B.word8 $ value8 t

-- | Tag for the 'Dynamic' constant pool entry.
data TDynamic = TDynamic
  deriving (Show)

instance Word8Constant TDynamic where
  value8 TDynamic = 17

instance Bytecode TDynamic where
  parser = TDynamic <$ word8 (value8 TDynamic)
  encode t = B.word8 $ value8 t

-- | Tag for the 'InvokeDynamic' constant pool entry.
data TInvokeDynamic = TInvokeDynamic
  deriving (Show)

instance Word8Constant TInvokeDynamic where
  value8 TInvokeDynamic = 18

instance Bytecode TInvokeDynamic where
  parser = TInvokeDynamic <$ word8 (value8 TInvokeDynamic)
  encode t = B.word8 $ value8 t

-- | Tag for the 'Module' constant pool entry.
data TModule = TModule
  deriving (Show)

instance Word8Constant TModule where
  value8 TModule = 19

instance Bytecode TModule where
  parser = TModule <$ word8 (value8 TModule)
  encode t = B.word8 $ value8 t

-- | Tag for the 'Package' constant pool entry.
data TPackage = TPackage
  deriving (Show)

instance Word8Constant TPackage where
  value8 TPackage = 20

instance Bytecode TPackage where
  parser = TPackage <$ word8 (value8 TPackage)
  encode t = B.word8 $ value8 t

--------------------------------------------------------------------------------
-- Constant Pool Entries
--------------------------------------------------------------------------------

-- | An entry of the constant pool.
data ConstantPoolEntry
  = -- | Class or an interface. Contains a reference to an 'Utf8'
    -- constant encoding the class/interface name.
    Class TClass Utf8Ref
  | -- | Represents a field. References the 'Class' it belongs to and a
    -- 'NameAndType' constant containing the field information.
    FieldRef TFieldRef ClassRef NameAndTypeRef
  | -- | Represents a method. References the 'Class' it belongs to and a
    -- 'NameAndType' constant containing the method information.
    MethodRef TMethodRef ClassRef NameAndTypeRef
  | -- | Represents an interface method. References the 'Class' it belongs to
    -- and a 'NameAndType' constant containing the interface method information.
    InterfaceMethodRef TInterfaceMethodRef ClassRef NameAndTypeRef
  | -- | Constant String object. Contains a reference to its 'Utf8'
    -- content.
    String TString Utf8Ref
  | -- | 32 bit integer constant (int).
    Integer TInteger Word32
  | -- | 32 bit floating point number constant (float).
    Float TFloat Word32
  | -- | 64 bit integer constant (long).
    Long TLong Word64
  | -- | 64 bit floating point number constant (double).
    Double TDouble Word64
  | -- | Represents a field/method without information about the class it
    -- belongs to. References the 'Utf8' field/method name and the 'Utf8'
    -- field/method descriptor.
    NameAndType TNameAndType Utf8Ref Utf8Ref
  | -- | String encoded with Modified UTF-8.
    Utf8 TUtf8 ByteString
  | -- | Method handle containing the reference kind and a reference to a
    -- 'ConstantPoolEntry' depending on the reference kind.
    MethodHandle TMethodHandle MethodRefKind ConstantPoolRef
  | -- | TODO
    MethodType TMethodType Utf8Ref
  | -- | TODO
    Dynamic TDynamic BootstrapMethodAttrRef NameAndTypeRef
  | -- | TODO
    InvokeDynamic TInvokeDynamic BootstrapMethodAttrRef NameAndTypeRef
  | -- | Represents a module. References its 'Utf8' name.
    Module TModule Utf8Ref
  | -- | Represents a package exported or opened by a module. References its
    -- 'Utf8' name.
    Package TPackage Utf8Ref
  deriving (Show)

instance Bytecode ConstantPoolEntry where
  parser =
    choice
      [ parserClass,
        parserFieldRef,
        parserMethodRef,
        parserInterfaceMethodRef,
        parserString,
        parserInteger,
        parserFloat,
        parserLong,
        parserDouble,
        parserNameAndType,
        parserUtf8,
        parserMethodHandle,
        parserMethodType,
        parserDynamic,
        parserInvokeDynamic,
        parserModule,
        parserPackage
      ]
    where
      parserClass = Class <$> parser <*> (Utf8Ref <$> anyWord16)
      parserFieldRef =
        FieldRef <$> parser <*> (ClassRef <$> anyWord16)
          <*> (NameAndTypeRef <$> anyWord16)
      parserMethodRef =
        MethodRef <$> parser <*> (ClassRef <$> anyWord16)
          <*> (NameAndTypeRef <$> anyWord16)
      parserInterfaceMethodRef =
        InterfaceMethodRef <$> parser <*> (ClassRef <$> anyWord16)
          <*> (NameAndTypeRef <$> anyWord16)
      parserString = String <$> parser <*> (Utf8Ref <$> anyWord16)
      parserInteger = Integer <$> parser <*> anyWord32
      parserFloat = Float <$> parser <*> anyWord32
      parserLong = Long <$> parser <*> anyWord64
      parserDouble = Double <$> parser <*> anyWord64
      parserNameAndType =
        NameAndType <$> parser <*> (Utf8Ref <$> anyWord16)
          <*> (Utf8Ref <$> anyWord16)
      parserUtf8 =
        Utf8 <$> parser <*> do
          length <- anyWord16
          BS.take (fromIntegral length)
      parserMethodHandle =
        MethodHandle <$> parser <*> parser <*> (ConstantPoolRef <$> anyWord16)
      parserMethodType = MethodType <$> parser <*> (Utf8Ref <$> anyWord16)
      parserDynamic =
        Dynamic <$> parser <*> anyWord16 <*> (NameAndTypeRef <$> anyWord16)
      parserInvokeDynamic =
        InvokeDynamic <$> parser <*> anyWord16
          <*> (NameAndTypeRef <$> anyWord16)
      parserModule = Module <$> parser <*> (Utf8Ref <$> anyWord16)
      parserPackage = Package <$> parser <*> (Utf8Ref <$> anyWord16)
  encode (Class t (Utf8Ref u)) = encode t <> word16BE u
  encode (FieldRef t (ClassRef c) (NameAndTypeRef n)) =
    encode t <> word16BE c <> word16BE n
  encode (MethodRef t (ClassRef c) (NameAndTypeRef n)) =
    encode t <> word16BE c <> word16BE n
  encode (InterfaceMethodRef t (ClassRef c) (NameAndTypeRef n)) =
    encode t <> word16BE c <> word16BE n
  encode (String t (Utf8Ref u)) = encode t <> word16BE u
  encode (Integer t w) = encode t <> word32BE w
  encode (Float t w) = encode t <> word32BE w
  encode (Long t w) = encode t <> word64BE w
  encode (Double t w) = encode t <> word64BE w
  encode (NameAndType t (Utf8Ref u) (Utf8Ref u')) =
    encode t <> word16BE u <> word16BE u'
  encode (Utf8 t b) =
    encode t <> word16BE (fromIntegral (Data.ByteString.length b))
      <> byteString b
  encode (MethodHandle t m (ConstantPoolRef c)) =
    encode t <> encode m <> word16BE c
  encode (MethodType t (Utf8Ref u)) = encode t <> word16BE u
  encode (Dynamic t b (NameAndTypeRef n)) = encode t <> word16BE b <> word16BE n
  encode (InvokeDynamic t b (NameAndTypeRef n)) =
    encode t <> word16BE b <> word16BE n
  encode (Module t (Utf8Ref u)) = encode t <> word16BE u
  encode (Package t (Utf8Ref u)) = encode t <> word16BE u

-- | Determines the constant pool size of entries. Bytecode has the questionable
-- feature that 'Double' and 'Long' entries take up two constant pool entry
-- slots instead of one; for parsing and encoding, we need to consider this.
constantPoolSize :: Num a => ConstantPoolEntry -> a
constantPoolSize (Double _ _) = 2
constantPoolSize (Long _ _) = 2
constantPoolSize _ = 1

-- | Types of methof references used by 'MethodHandle's.
--
-- JVM spec:
-- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-5.html#jvms-5.4.3.5
data MethodRefKind
  = -- | TODO
    GetField
  | -- | TODO
    GetStatic
  | -- | TODO
    PutField
  | -- | TODO
    PutStatic
  | -- | TODO
    InvokeVirtual
  | -- | TODO
    InvokeStatic
  | -- | TODO
    InvokeSpecial
  | -- | TODO
    NewInvokeSpecial
  | -- | TODO
    InvokeInterface
  deriving (Show)

instance Word8Constant MethodRefKind where
  value8 GetField = 1
  value8 GetStatic = 2
  value8 PutField = 3
  value8 PutStatic = 4
  value8 InvokeVirtual = 5
  value8 InvokeStatic = 6
  value8 InvokeSpecial = 7
  value8 NewInvokeSpecial = 8
  value8 InvokeInterface = 9

instance Bytecode MethodRefKind where
  parser =
    choice
      [ parserGetField,
        parserGetStatic,
        parserPutField,
        parserPutStatic,
        parserInvokeVirtual,
        parserInvokeStatic,
        parserInvokeSpecial,
        parserNewInvokeSpecial,
        parserInvokeInterface
      ]
    where
      parserGetField = GetField <$ word8 (value8 GetField)
      parserGetStatic = GetStatic <$ word8 (value8 GetStatic)
      parserPutField = PutField <$ word8 (value8 PutField)
      parserPutStatic = PutStatic <$ word8 (value8 PutStatic)
      parserInvokeVirtual = InvokeVirtual <$ word8 (value8 InvokeVirtual)
      parserInvokeStatic = InvokeStatic <$ word8 (value8 InvokeStatic)
      parserInvokeSpecial = InvokeSpecial <$ word8 (value8 InvokeSpecial)
      parserNewInvokeSpecial =
        NewInvokeSpecial <$ word8 (value8 NewInvokeSpecial)
      parserInvokeInterface = InvokeInterface <$ word8 (value8 InvokeInterface)
  encode m = B.word8 $ value8 m

--------------------------------------------------------------------------------
-- References
--------------------------------------------------------------------------------

-- | Reference to a 'Class' constant.
newtype ClassRef = ClassRef Word16

instance Show ClassRef where
  show (ClassRef n) = "#" ++ show n

-- | Reference to a 'NameAndType' constant.
newtype NameAndTypeRef = NameAndTypeRef Word16

instance Show NameAndTypeRef where
  show (NameAndTypeRef n) = "#" ++ show n

-- | Reference to an 'Utf8' constant.
newtype Utf8Ref = Utf8Ref Word16

instance Show Utf8Ref where
  show (Utf8Ref n) = "#" ++ show n

-- | General reference to a 'ConstantPoolEntry'.
newtype ConstantPoolRef = ConstantPoolRef Word16

instance Show ConstantPoolRef where
  show (ConstantPoolRef n) = "#" ++ show n

-- | Reference to a BootstrapMethod attribute
type BootstrapMethodAttrRef = Word16 -- TODO: move?
-- |
-- Module: Data.Oktade.Classfile.Metadata.Classfile
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile constant pool.
module Data.Oktade.Classfile.Metadata.ConstantPool
  ( -- * Constant Pool
    -- $constant-pool
    ConstantPool (..),

    -- ** Constant Pool Tags
    -- $constant-pool-tags
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
    constantPoolSize,
    MethodRefKind (..),

    -- *** References
    ClassRef (..),
    NameAndTypeRef (..),
    Utf8Ref (..),
    ConstantPoolRef (..),
    BootstrapMethodAttrRef,
  )
where

import Data.Attoparsec.ByteString.Lazy (choice, word8)
import qualified Data.Attoparsec.ByteString.Lazy as A (take)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (length)
import Data.ByteString.Builder (byteString, word16BE, word32BE, word64BE)
import qualified Data.ByteString.Builder as BB (word8)
import Data.IntMap (IntMap, fromAscList)
import Data.Oktade.ByteConstant (Word8Constant (..))
import Data.Oktade.ByteParser (anyWord16, anyWord32, anyWord64)
import Data.Oktade.Parse (Parse (..), Unparse (..))
import Data.Word (Word16, Word32, Word64)

--------------------------------------------------------------------------------
-- Constant Pool
--------------------------------------------------------------------------------

-- $constant-pool
-- The constant pool is arguably one of the larger metadata structures in the
-- classfile, as such the corresponding data structures may seem a bit
-- overwhelming at first.
--
-- As a top level overview, the constant pool is a map of indices to entries
-- (here, the map is the 'ConstantPool' data structure).
--
-- Each entry structure has a specific tag which allows parsers to determine
-- what kind of entry it should parse. These constants are described in the
-- __Constant Pool Tags__ section.
--
-- The actual constant pool entries are represented as 'ConstantPoolEntry's.
-- Most constant pool entries have a size of one entry, however some ('Long' and
-- 'Double', to be specific) take up two entries in the constant pool. oktade's
-- constant pool implementation accounts for that (after a night of despair
-- and confusion due to some classfiles not getting parsed correctly... Let this
-- be a lesson to read the JVM specification _carefully_).

-- | Represents the classfile constant pool which is a list of
-- 'ConstantPoolEntry's mapped to their indices.
--
-- More about the constant pool can be learned in the JVM specification:
-- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4
newtype ConstantPool = ConstantPool {entries :: IntMap ConstantPoolEntry}
  deriving (Show)

instance Parse ConstantPool where
  parser =
    ConstantPool <$> do
      entryCountPlusOne <- anyWord16
      -- Yes, this is correct. For some reason, the classfile stores the
      -- constant pool size + 1.
      let entryCount = fromIntegral entryCountPlusOne - 1
      entries' <- countC entryCount 1 parser []
      return $ fromAscList entries'
    where
      -- Custom 'count' function for considering the 'constantPoolSize' of
      -- entries.
      countC c n p acc
        | n > c = return acc
        | otherwise = do
          x <- p
          countC c (n + constantPoolSize x) p ((n, x) : acc)

instance Unparse ConstantPool where
  unparser (ConstantPool m) =
    -- Again this is correct. The classfile stores the constant pool size + 1.
    word16BE (sizeC m + 1) <> foldr ((<>) . unparser) mempty m
    where
      -- Custom 'size' function for considering the 'constantPoolSize' of
      -- entries.
      sizeC = foldr ((+) . constantPoolSize) 0

--------------------------------------------------------------------------------
-- Constant Pool Tags
--------------------------------------------------------------------------------

-- $constant-pool-tags
-- Each constant pool entry structure includes a tag for parsers to be able to
-- differentiate between different entry types. Tags are numeric 'Word8' values.
--
-- More about the constant pool entry tags can be learned in the
-- JVM specification:
-- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4

-- | Tag for the 'Class' constant pool entry.
data TClass = TClass
  deriving (Show)

instance Word8Constant TClass where
  value8 TClass = 7

instance Parse TClass where
  parser = TClass <$ word8 (value8 TClass)

instance Unparse TClass where
  unparser = BB.word8 . value8

-- | Tag for the 'FieldRef' constant pool entry.
data TFieldRef = TFieldRef
  deriving (Show)

instance Word8Constant TFieldRef where
  value8 TFieldRef = 9

instance Parse TFieldRef where
  parser = TFieldRef <$ word8 (value8 TFieldRef)

instance Unparse TFieldRef where
  unparser = BB.word8 . value8

-- | Tag for the 'MethodRef' constant pool entry.
data TMethodRef = TMethodRef
  deriving (Show)

instance Word8Constant TMethodRef where
  value8 TMethodRef = 10

instance Parse TMethodRef where
  parser = TMethodRef <$ word8 (value8 TMethodRef)

instance Unparse TMethodRef where
  unparser = BB.word8 . value8

-- | Tag for the 'InterfaceMethodRef' constant pool entry.
data TInterfaceMethodRef = TInterfaceMethodRef
  deriving (Show)

instance Word8Constant TInterfaceMethodRef where
  value8 TInterfaceMethodRef = 11

instance Parse TInterfaceMethodRef where
  parser = TInterfaceMethodRef <$ word8 (value8 TInterfaceMethodRef)

instance Unparse TInterfaceMethodRef where
  unparser = BB.word8 . value8

-- | Tag for the 'String' constant pool entry.
data TString = TString
  deriving (Show)

instance Word8Constant TString where
  value8 TString = 8

instance Parse TString where
  parser = TString <$ word8 (value8 TString)

instance Unparse TString where
  unparser = BB.word8 . value8

-- | Tag for the 'Integer' constant pool entry.
data TInteger = TInteger
  deriving (Show)

instance Word8Constant TInteger where
  value8 TInteger = 3

instance Parse TInteger where
  parser = TInteger <$ word8 (value8 TInteger)

instance Unparse TInteger where
  unparser = BB.word8 . value8

-- | Tag for the 'Float' constant pool entry.
data TFloat = TFloat
  deriving (Show)

instance Word8Constant TFloat where
  value8 TFloat = 4

instance Parse TFloat where
  parser = TFloat <$ word8 (value8 TFloat)

instance Unparse TFloat where
  unparser = BB.word8 . value8

-- | Tag for the 'Long' constant pool entry.
data TLong = TLong
  deriving (Show)

instance Word8Constant TLong where
  value8 TLong = 5

instance Parse TLong where
  parser = TLong <$ word8 (value8 TLong)

instance Unparse TLong where
  unparser = BB.word8 . value8

-- | Tag for the 'Double' constant pool entry.
data TDouble = TDouble
  deriving (Show)

instance Word8Constant TDouble where
  value8 TDouble = 6

instance Parse TDouble where
  parser = TDouble <$ word8 (value8 TDouble)

instance Unparse TDouble where
  unparser = BB.word8 . value8

-- | Tag for the 'NameAndType' constant pool entry.
data TNameAndType = TNameAndType
  deriving (Show)

instance Word8Constant TNameAndType where
  value8 TNameAndType = 12

instance Parse TNameAndType where
  parser = TNameAndType <$ word8 (value8 TNameAndType)

instance Unparse TNameAndType where
  unparser = BB.word8 . value8

-- | Tag for the 'Utf8' constant pool entry.
data TUtf8 = TUtf8
  deriving (Show)

instance Word8Constant TUtf8 where
  value8 TUtf8 = 1

instance Parse TUtf8 where
  parser = TUtf8 <$ word8 (value8 TUtf8)

instance Unparse TUtf8 where
  unparser = BB.word8 . value8

-- | Tag for the 'MethodHandle' constant pool entry.
data TMethodHandle = TMethodHandle
  deriving (Show)

instance Word8Constant TMethodHandle where
  value8 TMethodHandle = 15

instance Parse TMethodHandle where
  parser = TMethodHandle <$ word8 (value8 TMethodHandle)

instance Unparse TMethodHandle where
  unparser = BB.word8 . value8

-- | Tag for the 'MethodType' constant pool entry.
data TMethodType = TMethodType
  deriving (Show)

instance Word8Constant TMethodType where
  value8 TMethodType = 16

instance Parse TMethodType where
  parser = TMethodType <$ word8 (value8 TMethodType)

instance Unparse TMethodType where
  unparser = BB.word8 . value8

-- | Tag for the 'Dynamic' constant pool entry.
data TDynamic = TDynamic
  deriving (Show)

instance Word8Constant TDynamic where
  value8 TDynamic = 17

instance Parse TDynamic where
  parser = TDynamic <$ word8 (value8 TDynamic)

instance Unparse TDynamic where
  unparser = BB.word8 . value8

-- | Tag for the 'InvokeDynamic' constant pool entry.
data TInvokeDynamic = TInvokeDynamic
  deriving (Show)

instance Word8Constant TInvokeDynamic where
  value8 TInvokeDynamic = 18

instance Parse TInvokeDynamic where
  parser = TInvokeDynamic <$ word8 (value8 TInvokeDynamic)

instance Unparse TInvokeDynamic where
  unparser = BB.word8 . value8

-- | Tag for the 'Module' constant pool entry.
data TModule = TModule
  deriving (Show)

instance Word8Constant TModule where
  value8 TModule = 19

instance Parse TModule where
  parser = TModule <$ word8 (value8 TModule)

instance Unparse TModule where
  unparser = BB.word8 . value8

-- | Tag for the 'Package' constant pool entry.
data TPackage = TPackage
  deriving (Show)

instance Word8Constant TPackage where
  value8 TPackage = 20

instance Parse TPackage where
  parser = TPackage <$ word8 (value8 TPackage)

instance Unparse TPackage where
  unparser = BB.word8 . value8

--------------------------------------------------------------------------------
-- Constant Pool Entries
--------------------------------------------------------------------------------

-- | An entry of the constant pool.
--
-- The constant pool contains entries. Those entries can be instances of
-- different structures, defined in this data type. A general constant pool
-- entry consists of a 'Word8' tag and an arbitrary (but fixed) amount of bytes
-- storing data.
--
-- More about the constant pool entries can be learned in the JVM specification:
-- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4
data ConstantPoolEntry
  = -- | Class or an interface. Contains a reference to an 'Utf8'
    -- constant encoding the class/interface name.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.1
    Class TClass Utf8Ref
  | -- | Represents a field. References the 'Class' it belongs to and a
    -- 'NameAndType' constant containing the field information.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.2
    FieldRef TFieldRef ClassRef NameAndTypeRef
  | -- | Represents a method. References the 'Class' it belongs to and a
    -- 'NameAndType' constant containing the method information.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.2
    MethodRef TMethodRef ClassRef NameAndTypeRef
  | -- | Represents an interface method. References the 'Class' it belongs to
    -- and a 'NameAndType' constant containing the interface method information.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.2
    InterfaceMethodRef TInterfaceMethodRef ClassRef NameAndTypeRef
  | -- | Constant String object. Contains a reference to its 'Utf8'
    -- content.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.3
    String TString Utf8Ref
  | -- | 32 bit integer constant (int).
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.4
    Integer TInteger Word32
  | -- | 32 bit floating point number constant (float).
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.4
    Float TFloat Word32
  | -- | 64 bit integer constant (long).
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.5
    Long TLong Word64
  | -- | 64 bit floating point number constant (double).
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.5
    Double TDouble Word64
  | -- | Represents a field/method without information about the class it
    -- belongs to. References the 'Utf8' field/method name and the 'Utf8'
    -- field/method descriptor.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.6
    NameAndType TNameAndType Utf8Ref Utf8Ref
  | -- | String encoded with Modified UTF-8.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.7
    Utf8 TUtf8 ByteString
  | -- | Method handle containing the reference kind and a reference to a
    -- 'ConstantPoolEntry' depending on the reference kind.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.8
    MethodHandle TMethodHandle MethodRefKind ConstantPoolRef
  | -- | Represents a method type. The 'Utf8Ref' points to the method
    -- descriptor.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.9
    MethodType TMethodType Utf8Ref
  | -- | Represents a dynamically computed constant, determined when invoking a
    -- corresponding bootstrap method, for example when @ldc@ is called.
    -- Contains a reference to the constant's type.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.10
    Dynamic TDynamic BootstrapMethodAttrRef NameAndTypeRef
  | -- | Represents a dynamically computed call site, determined when invoking a
    -- corresponding bootstrap method, in this case when @invokedynamic@ is
    -- called. Contains a reference to the call site's type.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.10
    InvokeDynamic TInvokeDynamic BootstrapMethodAttrRef NameAndTypeRef
  | -- | Represents a module. References its 'Utf8' name.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.11
    Module TModule Utf8Ref
  | -- | Represents a package exported or opened by a module. References its
    -- 'Utf8' name.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.12
    Package TPackage Utf8Ref
  deriving (Show)

instance Parse ConstantPoolEntry where
  parser =
    let parsers =
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
     in choice parsers
    where
      parserClass = Class <$> parser <*> parser
      parserFieldRef = FieldRef <$> parser <*> parser <*> parser
      parserMethodRef = MethodRef <$> parser <*> parser <*> parser
      parserInterfaceMethodRef =
        InterfaceMethodRef <$> parser <*> parser <*> parser
      parserString = String <$> parser <*> parser
      parserInteger = Integer <$> parser <*> anyWord32
      parserFloat = Float <$> parser <*> anyWord32
      parserLong = Long <$> parser <*> anyWord64
      parserDouble = Double <$> parser <*> anyWord64
      parserNameAndType = NameAndType <$> parser <*> parser <*> parser
      parserUtf8 = Utf8 <$> parser <*> (anyWord16 >>= A.take . fromIntegral)
      parserMethodHandle = MethodHandle <$> parser <*> parser <*> parser
      parserMethodType = MethodType <$> parser <*> parser
      parserDynamic = Dynamic <$> parser <*> anyWord16 <*> parser
      parserInvokeDynamic = InvokeDynamic <$> parser <*> anyWord16 <*> parser
      parserModule = Module <$> parser <*> parser
      parserPackage = Package <$> parser <*> parser

instance Unparse ConstantPoolEntry where
  unparser (Class t u) = unparser t <> unparser u
  unparser (FieldRef t c n) = unparser t <> unparser c <> unparser n
  unparser (MethodRef t c n) = unparser t <> unparser c <> unparser n
  unparser (InterfaceMethodRef t c n) = unparser t <> unparser c <> unparser n
  unparser (String t u) = unparser t <> unparser u
  unparser (Integer t w) = unparser t <> word32BE w
  unparser (Float t w) = unparser t <> word32BE w
  unparser (Long t w) = unparser t <> word64BE w
  unparser (Double t w) = unparser t <> word64BE w
  unparser (NameAndType t u u') = unparser t <> unparser u <> unparser u'
  unparser (Utf8 t b) =
    unparser t <> word16BE (fromIntegral (B.length b)) <> byteString b
  unparser (MethodHandle t m c) = unparser t <> unparser m <> unparser c
  unparser (MethodType t u) = unparser t <> unparser u
  unparser (Dynamic t b n) = unparser t <> word16BE b <> unparser n
  unparser (InvokeDynamic t b n) = unparser t <> word16BE b <> unparser n
  unparser (Module t u) = unparser t <> unparser u
  unparser (Package t u) = unparser t <> unparser u

-- | Determines the constant pool size of entries. Bytecode has the questionable
-- feature that 'Double' and 'Long' entries take up two constant pool entry
-- slots instead of one; for parsing and unparsing, we need to account for this.
constantPoolSize :: Num a => ConstantPoolEntry -> a
constantPoolSize (Double _ _) = 2
constantPoolSize (Long _ _) = 2
constantPoolSize _ = 1

-- | Types of method references used by 'MethodHandle's.
--
-- Read the JVM specification for more information:
-- https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-5.html#jvms-5.4.3.5
data MethodRefKind
  = GetField
  | GetStatic
  | PutField
  | PutStatic
  | InvokeVirtual
  | InvokeStatic
  | InvokeSpecial
  | NewInvokeSpecial
  | InvokeInterface
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

instance Parse MethodRefKind where
  parser =
    let parsers =
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
     in choice parsers
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

instance Unparse MethodRefKind where
  unparser = BB.word8 . value8

--------------------------------------------------------------------------------
-- References
--------------------------------------------------------------------------------

-- | Reference to a 'Class' constant.
newtype ClassRef = ClassRef Word16
  deriving (Show)

instance Parse ClassRef where
  parser = ClassRef <$> anyWord16

instance Unparse ClassRef where
  unparser (ClassRef c) = word16BE c

-- | Reference to a 'NameAndType' constant.
newtype NameAndTypeRef = NameAndTypeRef Word16
  deriving (Show)

instance Parse NameAndTypeRef where
  parser = NameAndTypeRef <$> anyWord16

instance Unparse NameAndTypeRef where
  unparser (NameAndTypeRef n) = word16BE n

-- | Reference to an 'Utf8' constant.
newtype Utf8Ref = Utf8Ref Word16
  deriving (Show)

instance Parse Utf8Ref where
  parser = Utf8Ref <$> anyWord16

instance Unparse Utf8Ref where
  unparser (Utf8Ref u) = word16BE u

-- | General reference to a 'ConstantPoolEntry'.
newtype ConstantPoolRef = ConstantPoolRef Word16
  deriving (Show)

instance Parse ConstantPoolRef where
  parser = ConstantPoolRef <$> anyWord16

instance Unparse ConstantPoolRef where
  unparser (ConstantPoolRef c) = word16BE c

-- | Reference to a BootstrapMethod attribute.
type BootstrapMethodAttrRef = Word16 -- TODO: where does this belong to?

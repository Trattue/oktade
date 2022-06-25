-- |
-- Module: Data.Oktade.Classfile.Metadata.ConstantPool
-- License: Apache-2.0
--
-- Types and functions for parsing and unparsing the classfile constant pool.
module Data.Oktade.Classfile.Metadata.ConstantPool
  ( -- * Constant Pool
    -- $constant-pool
    ConstantPool (..),

    -- ** Constant Pool Entries
    ConstantPoolEntry (..),
    constantPoolSize,
    MethodRefKind (..),

    -- *** References
    ClassRef (..),
    NameAndTypeRef (..),
    Utf8Ref (..),
    PackageRef (..),
    ConstantPoolRef (..),
    BootstrapMethodAttrRef,
  )
where

import Data.Attoparsec.ByteString.Lazy (anyWord8, choice, word8)
import qualified Data.Attoparsec.ByteString.Lazy as A (take)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (length)
import Data.ByteString.Builder (byteString, word16BE, word32BE, word64BE)
import qualified Data.ByteString.Builder as BB (word8)
import Data.IntMap (IntMap, fromAscList)
import Data.Oktade.ByteConstant (Word8Constant (..))
import Data.Oktade.ByteParser (anyWord16, anyWord32, anyWord64)
import Data.Oktade.Parse (Parse (..), Unparse (..))
import Data.Word (Word16, Word32, Word64, Word8)

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
-- what kind of entry it should parse. Oktade uses them internally but hides
-- them from the user.
--
-- The actual constant pool entries are represented as 'ConstantPoolEntry's.
-- Most constant pool entries have a size of one entry, however some ('Long' and
-- 'Double', to be specific) take up two entries in the constant pool. Oktade's
-- constant pool implementation accounts for that (after a night of despair
-- and confusion due to some classfiles not getting parsed correctly... Let this
-- be a lesson to read the JVM specification /carefully/).

-- | Represents the classfile constant pool which is a list of
-- 'ConstantPoolEntry's mapped to their indices.
--
-- More about the constant pool can be learned in the JVM specification:
-- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4
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
    -- Again, this is correct. The classfile stores the constant pool size + 1.
    word16BE (sizeC m + 1) <> foldr ((<>) . unparser) mempty m
    where
      -- Custom 'size' function for considering the 'constantPoolSize' of
      -- entries.
      sizeC = foldr ((+) . constantPoolSize) 0

--------------------------------------------------------------------------------
-- Constant Pool Entries
--------------------------------------------------------------------------------

-- | An entry of the constant pool.
--
-- The constant pool contains entries; those can be instances of different
-- structures, defined in this data type. A general constant pool entry contains
-- one or several data fields.
--
-- NB: A general constant pool entry consists of a 'Word8' tag and an arbitrary
-- (but fixed) amount of bytes storing data. We only expose the parsed data, not
-- the tags (so you don't have to think about those).
--
-- More about the constant pool entries can be learned in the JVM specification:
-- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4
data ConstantPoolEntry
  = -- | Class or an interface. Contains a reference to an 'Utf8'
    -- constant encoding the class/interface name.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4.1
    Class Utf8Ref
  | -- | Represents a field. References the 'Class' it belongs to and a
    -- 'NameAndType' constant containing the field information.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4.2
    FieldRef ClassRef NameAndTypeRef
  | -- | Represents a method. References the 'Class' it belongs to and a
    -- 'NameAndType' constant containing the method information.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4.2
    MethodRef ClassRef NameAndTypeRef
  | -- | Represents an interface method. References the 'Class' it belongs to
    -- and a 'NameAndType' constant containing the interface method information.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4.2
    InterfaceMethodRef ClassRef NameAndTypeRef
  | -- | Constant String object. Contains a reference to its 'Utf8'
    -- content.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4.3
    String Utf8Ref
  | -- | 32 bit integer constant (int).
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4.4
    Integer Word32
  | -- | 32 bit floating point number constant (float).
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4.4
    Float Word32
  | -- | 64 bit integer constant (long).
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4.5
    Long Word64
  | -- | 64 bit floating point number constant (double).
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4.5
    Double Word64
  | -- | Represents a field/method without information about the class it
    -- belongs to. References the 'Utf8' field/method name and the 'Utf8'
    -- field/method descriptor.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4.6
    NameAndType Utf8Ref Utf8Ref
  | -- | String encoded with Modified UTF-8.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4.7
    Utf8 ByteString
  | -- | Method handle containing the reference kind and a reference to a
    -- 'ConstantPoolEntry' depending on the reference kind.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4.8
    MethodHandle MethodRefKind ConstantPoolRef
  | -- | Represents a method type. The 'Utf8Ref' points to the method
    -- descriptor.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4.9
    MethodType Utf8Ref
  | -- | Represents a dynamically computed constant, determined when invoking a
    -- corresponding bootstrap method, for example when @ldc@ is called.
    -- Contains a reference to the constant's type.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4.10
    Dynamic BootstrapMethodAttrRef NameAndTypeRef
  | -- | Represents a dynamically computed call site, determined when invoking a
    -- corresponding bootstrap method, in this case when @invokedynamic@ is
    -- called. Contains a reference to the call site's type.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4.10
    InvokeDynamic BootstrapMethodAttrRef NameAndTypeRef
  | -- | Represents a module. References its 'Utf8' name.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4.11
    Module Utf8Ref
  | -- | Represents a package exported or opened by a module. References its
    -- 'Utf8' name.
    --
    -- Read the JVM specification for more information:
    -- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.4.12
    Package Utf8Ref
  deriving (Show, Eq)

instance Parse ConstantPoolEntry where
  parser = anyWord8 >>= parser'
    where
      parser' t
        -- Guards map the tag to the parser of the struct. This is used over
        -- attoparsec's choice function to improve performance.
        | t == classTag = parserClass
        | t == fieldRefTag = parserFieldRef
        | t == methodRefTag = parserMethodRef
        | t == interfaceMethodRefTag = parserInterfaceMethodRef
        | t == stringTag = parserString
        | t == integerTag = parserInteger
        | t == floatTag = parserFloat
        | t == longTag = parserLong
        | t == doubleTag = parserDouble
        | t == nameAndTypeTag = parserNameAndType
        | t == utf8Tag = parserUtf8
        | t == methodHandleTag = parserMethodHandle
        | t == methodTypeTag = parserMethodType
        | t == dynamicTag = parserDynamic
        | t == invokeDynamicTag = parserInvokeDynamic
        | t == moduleTag = parserModule
        | t == packageTag = parserPackage
        | otherwise = fail "Unknown constant pool stack"
      parserClass = Class <$> parser
      parserFieldRef = FieldRef <$> parser <*> parser
      parserMethodRef = MethodRef <$> parser <*> parser
      parserInterfaceMethodRef = InterfaceMethodRef <$> parser <*> parser
      parserString = String <$> parser
      parserInteger = Integer <$> anyWord32
      parserFloat = Float <$> anyWord32
      parserLong = Long <$> anyWord64
      parserDouble = Double <$> anyWord64
      parserNameAndType = NameAndType <$> parser <*> parser
      parserUtf8 = Utf8 <$> (anyWord16 >>= A.take . fromIntegral)
      parserMethodHandle = MethodHandle <$> parser <*> parser
      parserMethodType = MethodType <$> parser
      parserDynamic = Dynamic <$> anyWord16 <*> parser
      parserInvokeDynamic = InvokeDynamic <$> anyWord16 <*> parser
      parserModule = Module <$> parser
      parserPackage = Package <$> parser

instance Unparse ConstantPoolEntry where
  unparser (Class u) = BB.word8 classTag <> unparser u
  unparser (FieldRef c n) = BB.word8 fieldRefTag <> unparser c <> unparser n
  unparser (MethodRef c n) = BB.word8 methodRefTag <> unparser c <> unparser n
  unparser (InterfaceMethodRef c n) =
    BB.word8 interfaceMethodRefTag <> unparser c <> unparser n
  unparser (String u) = BB.word8 stringTag <> unparser u
  unparser (Integer w) = BB.word8 integerTag <> word32BE w
  unparser (Float w) = BB.word8 floatTag <> word32BE w
  unparser (Long w) = BB.word8 longTag <> word64BE w
  unparser (Double w) = BB.word8 doubleTag <> word64BE w
  unparser (NameAndType u u') =
    BB.word8 nameAndTypeTag <> unparser u <> unparser u'
  unparser (Utf8 b) =
    BB.word8 utf8Tag <> word16BE (fromIntegral $ BS.length b) <> byteString b
  unparser (MethodHandle m c) =
    BB.word8 methodHandleTag <> unparser m <> unparser c
  unparser (MethodType u) = BB.word8 methodTypeTag <> unparser u
  unparser (Dynamic b n) = BB.word8 dynamicTag <> word16BE b <> unparser n
  unparser (InvokeDynamic b n) =
    BB.word8 invokeDynamicTag <> word16BE b <> unparser n
  unparser (Module u) = BB.word8 moduleTag <> unparser u
  unparser (Package u) = BB.word8 packageTag <> unparser u

-- | Tag for the 'Utf8' constant pool entry.
utf8Tag :: Word8
utf8Tag = 1

-- | Tag for the 'Integer' constant pool entry.
integerTag :: Word8
integerTag = 3

-- | Tag for the 'Float' constant pool entry.
floatTag :: Word8
floatTag = 4

-- | Tag for the 'Long' constant pool entry.
longTag :: Word8
longTag = 5

-- | Tag for the 'Double' constant pool entry.
doubleTag :: Word8
doubleTag = 6

-- | Tag for the 'Class' constant pool entry.
classTag :: Word8
classTag = 7

-- | Tag for the 'String' constant pool entry.
stringTag :: Word8
stringTag = 8

-- | Tag for the 'FieldRef' constant pool entry.
fieldRefTag :: Word8
fieldRefTag = 9

-- | Tag for the 'MethodRef' constant pool entry.
methodRefTag :: Word8
methodRefTag = 10

-- | Tag for the 'InterfaceMethodRef' constant pool entry.
interfaceMethodRefTag :: Word8
interfaceMethodRefTag = 11

-- | Tag for the 'NameAndType' constant pool entry.
nameAndTypeTag :: Word8
nameAndTypeTag = 12

-- | Tag for the 'MethodHandle' constant pool entry.
methodHandleTag :: Word8
methodHandleTag = 15

-- | Tag for the 'MethodType' constant pool entry.
methodTypeTag :: Word8
methodTypeTag = 16

-- | Tag for the 'Dynamic' constant pool entry.
dynamicTag :: Word8
dynamicTag = 17

-- | Tag for the 'InvokeDynamic' constant pool entry.
invokeDynamicTag :: Word8
invokeDynamicTag = 18

-- | Tag for the 'Module' constant pool entry.
moduleTag :: Word8
moduleTag = 18

-- | Tag for the 'Package' constant pool entry.
packageTag :: Word8
packageTag = 18

-- | Determines the constant pool size of entries. Bytecode has the questionable
-- feature that 'Double' and 'Long' entries take up two constant pool entry
-- slots instead of one; for parsing and unparsing, we need to account for this.
constantPoolSize :: Num a => ConstantPoolEntry -> a
constantPoolSize (Double _) = 2
constantPoolSize (Long _) = 2
constantPoolSize _ = 1

-- | Types of method references used by 'MethodHandle's.
--
-- Read the JVM specification for more information:
-- https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-5.html#jvms-5.4.3.5
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
  deriving (Show, Eq)

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
  deriving (Show, Eq)

instance Parse ClassRef where
  parser = ClassRef <$> anyWord16

instance Unparse ClassRef where
  unparser (ClassRef c) = word16BE c

-- | Reference to a 'NameAndType' constant.
newtype NameAndTypeRef = NameAndTypeRef Word16
  deriving (Show, Eq)

instance Parse NameAndTypeRef where
  parser = NameAndTypeRef <$> anyWord16

instance Unparse NameAndTypeRef where
  unparser (NameAndTypeRef n) = word16BE n

-- | Reference to an 'Utf8' constant.
newtype Utf8Ref = Utf8Ref Word16
  deriving (Show, Eq)

instance Parse Utf8Ref where
  parser = Utf8Ref <$> anyWord16

instance Unparse Utf8Ref where
  unparser (Utf8Ref u) = word16BE u

-- | Reference to an 'Package' constant.
newtype PackageRef = PackageRef Word16
  deriving (Show, Eq)

instance Parse PackageRef where
  parser = PackageRef <$> anyWord16

instance Unparse PackageRef where
  unparser (PackageRef u) = word16BE u

-- | General reference to a 'ConstantPoolEntry'.
newtype ConstantPoolRef = ConstantPoolRef Word16
  deriving (Show, Eq)

instance Parse ConstantPoolRef where
  parser = ConstantPoolRef <$> anyWord16

instance Unparse ConstantPoolRef where
  unparser (ConstantPoolRef c) = word16BE c

-- | Reference to a BootstrapMethod attribute.
type BootstrapMethodAttrRef = Word16 -- TODO: where does this belong to?

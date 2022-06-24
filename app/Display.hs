module Display where

import Data.Char (toLower)
import Data.IntMap (size, toAscList)
import Data.List (intercalate)
import Data.Oktade.ByteConstant (value8)
import Data.Oktade.Classfile (Classfile (..))
import Data.Oktade.Classfile.Class (Class (..))
import Data.Oktade.Classfile.Class.AccessFlags (AccessFlags (..))
import Data.Oktade.Classfile.Class.Attributes
  ( Attribute (..),
    Attributes (..),
    InnerClass (..),
    NDeprecated (..),
    NEnclosingMethod (..),
    NInnerClasses (..),
    NSourceDebugExtension (..),
    NSourceFile (..),
    NSynthetic (..),
  )
import Data.Oktade.Classfile.Class.Fields (Field (..), Fields (..))
import Data.Oktade.Classfile.Class.Fields.AccessFlags (FieldAccessFlags (..))
import Data.Oktade.Classfile.Class.Fields.Attributes
  ( FieldAttribute,
    FieldAttributes (FieldAttributes),
  )
import qualified Data.Oktade.Classfile.Class.Fields.Attributes as FA
  ( FieldAttribute (..),
  )
import Data.Oktade.Classfile.Class.Interfaces (Interfaces (..))
import Data.Oktade.Classfile.Class.Methods (Method (..), Methods (..))
import Data.Oktade.Classfile.Class.Methods.AccessFlags (MethodAccessFlags (..))
import Data.Oktade.Classfile.Class.Methods.Attributes
  ( MethodAttribute,
    MethodAttributes (MethodAttributes),
  )
import qualified Data.Oktade.Classfile.Class.Methods.Attributes as MA
  ( MethodAttribute (..),
  )
import Data.Oktade.Classfile.Class.SuperClass (SuperClass (..))
import Data.Oktade.Classfile.Class.ThisClass (ThisClass (..))
import Data.Oktade.Classfile.Metadata (Metadata (..))
import Data.Oktade.Classfile.Metadata.ConstantPool
  ( ClassRef (..),
    ConstantPool (..),
    ConstantPoolEntry (..),
    ConstantPoolRef (..),
    NameAndTypeRef (..),
    Utf8Ref (..),
  )
import Data.Oktade.Classfile.Metadata.Version (Version (..))
import Text.Printf (printf)

class Display a where
  display :: a -> String

instance Display Classfile where
  display (Classfile m c) = display m ++ display c

instance Display Metadata where
  display (Metadata v c) =
    "Metadata:\n" ++ indent [display v, display c]

instance Display Version where
  display (Version major minor) = "Version: " ++ show major ++ "." ++ show minor

instance Display ConstantPool where
  display (ConstantPool m)
    | null m = "Constant Pool: -"
    | otherwise =
      let l = size m
          -- Number of digits of the largest constant pool entry index.
          d = 1 + ceiling (logBase 10 $ fromIntegral l)
       in "Constant Pool:\n" ++ indent (showEntry l d <$> toAscList m)
    where
      -- Right aligned formatting considering the maximum number of digits.
      showEntry _ d (k, v) =
        printf ("%" ++ show d ++ "s") ("#" ++ show k) ++ " = " ++ display v

instance Display ConstantPoolEntry where
  display (Data.Oktade.Classfile.Metadata.ConstantPool.Class u) =
    "Class " ++ display u
  display (FieldRef c n) = "FieldRef " ++ display c ++ " " ++ display n
  display (MethodRef c n) = "MethodRef " ++ display c ++ " " ++ display n
  display (InterfaceMethodRef c n) =
    "FieldRef " ++ display c ++ " " ++ display n
  display (String u) = "String " ++ display u
  display (Integer w) = "Integer " ++ show w
  display (Float w) = "Float " ++ show w
  display (Long w) = "Long " ++ show w
  display (Double w) = "Double " ++ show w
  display (NameAndType u u') =
    "NameAndType " ++ display u ++ " " ++ display u'
  display (Utf8 b) = "Utf8 " ++ show b
  display (MethodHandle m c) =
    "MethodHandle " ++ show (value8 m) ++ " " ++ display c
  display (MethodType u) = "MethodType " ++ show u
  display (Dynamic b n) = "Dynamic " ++ show b ++ " " ++ display n
  display (InvokeDynamic b n) = "InvokeDynamic " ++ show b ++ " " ++ display n
  display (Module u) = "Module " ++ display u
  display (Package u) = "Package " ++ display u

instance Display ClassRef where
  display (ClassRef n) = "#" ++ show n

instance Display NameAndTypeRef where
  display (NameAndTypeRef n) = "#" ++ show n

instance Display Utf8Ref where
  display (Utf8Ref n) = "#" ++ show n

instance Display ConstantPoolRef where
  display (ConstantPoolRef n) = "#" ++ show n

instance Display Class where
  display c =
    let components =
          [ display $ accessFlags c,
            display $ this c,
            display $ super c,
            display $ interfaces c,
            display $ fields c,
            display $ methods c,
            display $ attributes c
          ]
     in "Class:\n" ++ indent components

instance Display AccessFlags where
  display (AccessFlags []) = "Access flags: -"
  display (AccessFlags as) =
    "Access flags: " ++ intercalate ", " ((toLower <$>) . show <$> as)

instance Display ThisClass where
  display (ThisClass c) = "This class: " ++ display c

instance Display SuperClass where
  display (SuperClass c) = "Super class: " ++ display c
  display Object = "Super class: -"

instance Display Interfaces where
  display (Interfaces []) = "Interfaces: -"
  display (Interfaces is) = "Interfaces: " ++ intercalate ", " (show <$> is)

instance Display Fields where
  display (Fields []) = "Fields: -"
  display (Fields fs) = "Fields:\n" ++ indent (display <$> fs)

instance Display Field where
  display (Field a u u' as) =
    "Field:\n"
      ++ indent
        [ display a,
          "Name: " ++ display u,
          "Descriptor: " ++ display u',
          display as
        ]

instance Display FieldAccessFlags where
  display (FieldAccessFlags []) = "Access flags: -"
  display (FieldAccessFlags as) =
    "Access flags: " ++ intercalate ", " ((toLower <$>) . show <$> as)

instance Display FieldAttributes where
  display (FieldAttributes []) = "Attributes: -"
  display (FieldAttributes as) = "Attributes:\n" ++ indent (display <$> as)

instance Display FieldAttribute where
  display (FA.Unknown u bs) = "Unknown " ++ display u ++ " " ++ show bs

instance Display Methods where
  display (Methods []) = "Methods: -"
  display (Methods ms) = "Methods:\n" ++ indent (display <$> ms)

instance Display Method where
  display (Method a u u' as) =
    "Method:\n"
      ++ indent
        [ display a,
          "Name: " ++ display u,
          "Descriptor: " ++ display u',
          display as
        ]

instance Display MethodAccessFlags where
  display (MethodAccessFlags []) = "Access flags: -"
  display (MethodAccessFlags as) =
    "Access flags: " ++ intercalate ", " ((toLower <$>) . show <$> as)

instance Display MethodAttributes where
  display (MethodAttributes []) = "Attributes: -"
  display (MethodAttributes as) = "Attributes:\n" ++ indent (display <$> as)

instance Display MethodAttribute where
  display (MA.Unknown u bs) = "Unknown " ++ display u ++ " " ++ show bs

instance Display Attributes where
  display (Attributes []) = "Attributes: -"
  display (Attributes as) = "Attributes:\n" ++ indent (display <$> as)

instance Display Attribute where
  display (SourceFile (NSourceFile u) u') =
    "SourceFile " ++ display u ++ " " ++ display u'
  display (InnerClasses (NInnerClasses u) is) =
    "InnerClasses " ++ display u ++ "\n" ++ indent (display <$> is)
  display (EnclosingMethod (NEnclosingMethod u) c n') =
    "EnclosingMethod " ++ display u ++ " " ++ display c ++ display n'
  display (SourceDebugExtension (NSourceDebugExtension u) bs) =
    "SourceDebugExtension " ++ display u ++ " " ++ show bs
  display (Synthetic (NSynthetic u)) = "Synthetic " ++ display u
  display (Deprecated (NDeprecated u)) = "Deprecated " ++ display u
  display (Unknown u bs) = "Unknown " ++ display u ++ " " ++ show bs

instance Display InnerClass where
  display (InnerClass i o n f) =
    "Inner Class: "
      ++ display i
      ++ " "
      ++ display o
      ++ " "
      ++ display n
      ++ " "
      ++ show f

indent :: [String] -> String
indent xs = unlines (go <$> xs)
  where
    go = init . unlines . (("  " ++) <$>) . lines

indent' :: String -> String
indent' s = indent [s]

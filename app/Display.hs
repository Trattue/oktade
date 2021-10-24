module Display where

import Data.Char (toUpper)
import Data.IntMap (size, toAscList)
import Data.Oktade.ByteConstant (Word32Constant (value32), Word8Constant (value8))
import Data.Oktade.Classfile (Classfile (..))
import Data.Oktade.Classfile.Class (Class (..))
import Data.Oktade.Classfile.Metadata (Metadata (..))
import Data.Oktade.Classfile.Metadata.ConstantPool
  ( ClassRef (..),
    ConstantPool (..),
    ConstantPoolEntry (..),
    ConstantPoolRef (..),
    NameAndTypeRef (..),
    Utf8Ref (..),
  )
import Data.Oktade.Classfile.Metadata.MagicNumber (MagicNumber)
import Data.Oktade.Classfile.Metadata.Version (Version (Version))
import Numeric (showHex)
import Text.Printf (printf)

class Display a where
  display :: a -> String

instance Display Classfile where
  display (Classfile m c) = display m ++ display c

instance Display Metadata where
  display (Metadata m v c) =
    "Metadata:\n" ++ indent [display m, display v, display c]

instance Display MagicNumber where
  display m = "Magic number: 0x" ++ (toUpper <$> showHex (value32 m) "")

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
      showEntry l d (k, v) =
        printf ("%" ++ show d ++ "s") ("#" ++ show k) ++ " = " ++ display v

instance Display ConstantPoolEntry where
  display (Data.Oktade.Classfile.Metadata.ConstantPool.Class t u) =
    "Class (" ++ show (value8 t) ++ ") " ++ display u
  display (FieldRef t c n) =
    "FieldRef (" ++ show (value8 t) ++ ") " ++ display c ++ " " ++ display n
  display (MethodRef t c n) =
    "MethodRef ("
      ++ show (value8 t)
      ++ ") "
      ++ display c
      ++ " "
      ++ display n
  display (InterfaceMethodRef t c n) =
    "FieldRef (" ++ show (value8 t) ++ ") " ++ display c ++ " " ++ display n
  display (String t u) = "String (" ++ show (value8 t) ++ ") " ++ display u
  display (Integer t w) = "Integer (" ++ show (value8 t) ++ ") " ++ show w
  display (Float t w) = "Float (" ++ show (value8 t) ++ ") " ++ show w
  display (Long t w) = "Long (" ++ show (value8 t) ++ ") " ++ show w
  display (Double t w) = "Double (" ++ show (value8 t) ++ ") " ++ show w
  display (NameAndType t u u') =
    "NameAndType (" ++ show (value8 t) ++ ") " ++ display u ++ " " ++ display u'
  display (Utf8 t b) = "Utf8 (" ++ show (value8 t) ++ ") " ++ show b
  display (MethodHandle t m c) =
    "MethodHandle ("
      ++ show (value8 t)
      ++ ") "
      ++ show (value8 m)
      ++ " "
      ++ display c
  display (MethodType t u) = "MethodType (" ++ show (value8 t) ++ ") " ++ show u
  display (Dynamic t b n) =
    "Dynamic (" ++ show (value8 t) ++ ") " ++ show b ++ " " ++ display n
  display (InvokeDynamic t b n) =
    "InvokeDynamic (" ++ show (value8 t) ++ ") " ++ show b ++ " " ++ display n
  display (Module t u) = "Module (" ++ show (value8 t) ++ ") " ++ display u
  display (Package t u) = "Package (" ++ show (value8 t) ++ ") " ++ display u

instance Display ClassRef where
  display (ClassRef n) = "#" ++ show n

instance Display NameAndTypeRef where
  display (NameAndTypeRef n) = "#" ++ show n

instance Display Utf8Ref where
  display (Utf8Ref n) = "#" ++ show n

instance Display ConstantPoolRef where
  display (ConstantPoolRef n) = "#" ++ show n

instance Display Class where
  display c = "Class:" -- TODO

indent :: [String] -> String
indent xs = unlines (go <$> xs)
  where
    go = init . unlines . (("  " ++) <$>) . lines

indent' :: String -> String
indent' s = indent [s]

module Types where

import Latte.Abs

type Pos = BNFC'Position

data CType = CInt | CStr | CBool | CVoid | CFun CType [CType]
  deriving (Eq)

getCType :: Type -> CType
getCType (Int _) = CInt
getCType (Str _) = CStr
getCType (Bool _) = CBool
getCType (Void _) = CVoid
getCType (Fun _ retType args) = CFun (getCType retType) (map getCType args)

instance Show CType where
  show CInt = "i32"
  show CStr = "string"
  show CBool = "i1"
  show CVoid = "void"
  show (CFun _ _) = "function"

typeToLLVM :: Type -> String
typeToLLVM (Int _) = "i32"
typeToLLVM (Str _) = "todo"
typeToLLVM (Bool _) = "i1"
typeToLLVM (Void _) = "todo"
typeToLLVM (Fun _ retType args) = "todo"

data Register = Reg Int deriving (Eq)

instance Show Register where
  show (Reg num) = "%r" ++ show num

nextReg :: Register -> Register
nextReg (Reg num) = Reg $ num + 1

data Val
  = IntVal Int
  | RegVal Register
  deriving (Eq)

instance Show Val where
  show (IntVal i) = show i
  show (RegVal reg) = show reg
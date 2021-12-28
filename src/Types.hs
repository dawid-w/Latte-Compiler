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

typeToLL :: Type -> String
typeToLL (Int _) = "i32"
typeToLL (Str _) = "todo"
typeToLL (Bool _) = "i1"
typeToLL (Void _) = "todo"
typeToLL (Fun _ retType args) = "todo"
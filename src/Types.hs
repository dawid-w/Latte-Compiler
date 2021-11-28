module Types where

import Latte.Abs

type Pos = BNFC'Position

-- data ValType = IntT | BoolT | ListT | VoidT | StringT
--   deriving (Eq)

data CType = CInt | CStr | CBool | CVoid | CFun CType [CType]
  deriving (Eq)

getCType :: Type -> CType
getCType (Int _) = CInt
getCType (Str _) = CStr
getCType (Bool _) = CBool
getCType (Void _) = CVoid
getCType (Fun _ retType args) = CFun (getCType retType) (map getCType args)

instance Show CType where
  show CInt = "int"
  show CStr = "string"
  show CBool = "bool"
  show CVoid = "void"
  show (CFun _ _) = "function"

-- data Val
--   = IntV Integer
--   | BoolV Bool
--   | ListV [Integer]
--   | StringV String
--   | VoidV

-- isBool :: Val -> Bool
-- isBool (BoolV b) = True
-- isBool _ = False

-- isInt :: Val -> Bool
-- isInt (IntV b) = True
-- isInt _ = False

-- isString :: Val -> Bool
-- isString (StringV b) = True
-- isString _ = False

-- getValType :: Val -> ValType
-- getValType (IntV i) = IntT
-- getValType (BoolV i) = BoolT
-- getValType (StringV i) = StringT
-- getValType VoidV = VoidT

-- typeToValType :: Type -> ValType
-- typeToValType (Int _) = IntT
-- typeToValType (Bool _) = BoolT
-- typeToValType (String _) = StringT
-- typeToValType (Void _) = VoidT

-- showValType :: Val -> String
-- showValType v = show $ getValType v

-- data EX = None | RT Val Pos
--   deriving (Show)

-- data TQ = Default | RO
--   deriving (Eq)

-- getIntVal :: Val -> Integer
-- getIntVal (IntV x) = x

-- getBoolVal :: Val -> Bool
-- getBoolVal (BoolV x) = x

-- instance Show Val where
--   show (IntV x) = show x
--   show (BoolV b) = show b
--   show (ListV l) = show l
--   show (StringV s) = s
--   show VoidV = ""

-- instance Eq Val where
--   (==) (IntV v1) (IntV v2) = v1 == v2
--   (==) (BoolV v1) (BoolV v2) = v1 == v2
--   (==) (StringV v1) (StringV v2) = v1 == v2

-- instance Ord Val where
--   (<) (IntV v1) (IntV v2) = v1 < v2
--   (>) (IntV v1) (IntV v2) = v1 > v2
--   (>=) (IntV v1) (IntV v2) = v1 >= v2
--   (<=) (IntV v1) (IntV v2) = v1 <= v2

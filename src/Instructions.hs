{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Instructions where

import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map
import Latte.Abs
import Types

data Instruction
  = ArtI ArtOp Val Val Register
  | CmpI RelOp CType Val Val Register
  | BrI Register Label Label
  | JmpI Label
  | IfElseI Register Label Label Label String String
  | WhileI Register String Label Label Label String
  | AddV Var CType
  | AddInit Var CType
  | GetV Var CType Register
  | SetV Var CType Register
  | BoolI Register BoolOp Val Val
  deriving (Eq)

instance Show Instruction where
  show (ArtI op v1 v2 reg) = show reg ++ " = " ++ show op ++ " i32 " ++ show v1 ++ ", " ++ show v2 ++ "\n"
  show (CmpI op ctype v1 v2 reg) = show reg ++ " = icmp " ++ relOpToLLVM op ++ " " ++ show ctype ++ " " ++ show v1 ++ ", " ++ show v2 ++ "\n"
  show (BrI reg label1 label2) = "br i1 " ++ show reg ++ ", label " ++ "%" ++ show label1 ++ ", label " ++ "%" ++ show label2 ++ "\n"
  show (JmpI label) = "br label %" ++ show label ++ "\n"
  show (IfElseI exprReg lTrue lFalse lEnd trueCode falseCode) = show (BrI exprReg lTrue lFalse) ++ show lTrue ++ ": \n" ++ trueCode ++ show (JmpI lEnd) ++ show lFalse ++ ":\n" ++ falseCode ++ show (JmpI lEnd) ++ show lEnd ++ ":\n"
  show (WhileI exprReg exprCode lStart lTrue lEnd code) = show (JmpI lStart) ++ show lStart ++ ": \n" ++ exprCode ++ show (BrI exprReg lTrue lEnd) ++ show lTrue ++ ":\n" ++ code ++ show (JmpI lStart) ++ show lEnd ++ ": \n"
  show (AddV var ctype) = show var ++ " = alloca " ++ show ctype ++ "\n"
  show (AddInit var ctype) = show var ++ " = alloca " ++ show ctype ++ "\n  store " ++ show ctype ++ " 0, " ++ show ctype ++ "* " ++ show var ++ "\n"
  show (GetV var ctype reg) = show reg ++ " = load " ++ show ctype ++ ", " ++ show ctype ++ "* " ++ show var ++ "\n"
  show (SetV var ctype reg) = "store " ++ show ctype ++ " " ++ show reg ++ ", " ++ show ctype ++ "* " ++ show var ++ "\n"
  show (BoolI reg op v1 v2) = show reg ++ " = " ++ show op ++ " i1 " ++ show v1 ++ ", " ++ show v2 ++ "\n"

data BoolOp
  = AndOp
  | OrOp
  | XorOp
  deriving (Eq)

instance Show BoolOp where
  show AndOp = "and"
  show OrOp = "or"
  show XorOp = "xor"

data ArtOp
  = AddOp
  | SubOp
  | DivOp
  | MulOp
  | ModOp
  deriving (Eq)

instance Show ArtOp where
  show AddOp = "add"
  show SubOp = "sub"
  show DivOp = "sdiv"
  show MulOp = "mul"
  show ModOp = "srem"

relOpToLLVM :: RelOp -> String
relOpToLLVM (LTH _) = "slt"
relOpToLLVM (LE _) = "sle"
relOpToLLVM (GTH _) = "sgt"
relOpToLLVM (GE _) = "sge"
relOpToLLVM (EQU _) = "eq"
relOpToLLVM (NE _) = "ne"

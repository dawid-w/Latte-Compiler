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
  deriving (Eq)

instance Show Instruction where
  show (ArtI op v1 v2 reg) = show reg ++ " = " ++ show op ++ " i32 " ++ show v1 ++ ", " ++ show v2 ++ "\n"
  show (CmpI op ctype v1 v2 reg) = show reg ++ " = icmp " ++ relOpToLLVM op ++ " " ++ show ctype ++ " " ++ show v1 ++ ", " ++ show v2 ++ "\n"
  show (BrI reg label1 label2) = "br i1 " ++ show reg ++ ", label " ++ "%" ++ show label1 ++ ", label " ++ "%" ++ show label2 ++ "\n"
  show (JmpI label) = "br label %" ++ show label ++ "\n"
  show (IfElseI exprReg lTrue lFalse lEnd trueCode falseCode) = (show (BrI exprReg lTrue lFalse) ++ show lTrue ++ ": \n" ++ trueCode ++ show (JmpI lEnd) ++ show lFalse ++ ":\n" ++ falseCode ++ show (JmpI lEnd) ++ show lEnd ++ ":\n")

data ArtOp
  = AddOp
  | SubOp
  deriving (Eq)

instance Show ArtOp where
  show AddOp = "add"
  show SubOp = "sub"

relOpToLLVM :: RelOp -> String
relOpToLLVM (LTH _) = "slt"
relOpToLLVM (LE _) = "sle"
relOpToLLVM (GTH _) = "sgt"
relOpToLLVM (GE _) = "sge"
relOpToLLVM (EQU _) = "eq"
relOpToLLVM (NE _) = "ne"

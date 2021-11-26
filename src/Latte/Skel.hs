-- File generated by the BNF Converter (bnfc 2.9.3).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Latte.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Latte.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: Latte.Abs.Ident -> Result
transIdent x = case x of
  Latte.Abs.Ident string -> failure x

transProgram :: Latte.Abs.Program -> Result
transProgram x = case x of
  Latte.Abs.Program topdefs -> failure x

transTopDef :: Latte.Abs.TopDef -> Result
transTopDef x = case x of
  Latte.Abs.FnDef type_ ident args block -> failure x

transArg :: Latte.Abs.Arg -> Result
transArg x = case x of
  Latte.Abs.Arg type_ ident -> failure x

transBlock :: Latte.Abs.Block -> Result
transBlock x = case x of
  Latte.Abs.Block stmts -> failure x

transStmt :: Latte.Abs.Stmt -> Result
transStmt x = case x of
  Latte.Abs.Empty -> failure x
  Latte.Abs.BStmt block -> failure x
  Latte.Abs.Decl type_ items -> failure x
  Latte.Abs.Ass ident expr -> failure x
  Latte.Abs.Incr ident -> failure x
  Latte.Abs.Decr ident -> failure x
  Latte.Abs.Ret expr -> failure x
  Latte.Abs.VRet -> failure x
  Latte.Abs.Cond expr stmt -> failure x
  Latte.Abs.CondElse expr stmt1 stmt2 -> failure x
  Latte.Abs.While expr stmt -> failure x
  Latte.Abs.SExp expr -> failure x

transItem :: Latte.Abs.Item -> Result
transItem x = case x of
  Latte.Abs.NoInit ident -> failure x
  Latte.Abs.Init ident expr -> failure x

transType :: Latte.Abs.Type -> Result
transType x = case x of
  Latte.Abs.Int -> failure x
  Latte.Abs.Str -> failure x
  Latte.Abs.Bool -> failure x
  Latte.Abs.Void -> failure x
  Latte.Abs.Fun type_ types -> failure x

transExpr :: Latte.Abs.Expr -> Result
transExpr x = case x of
  Latte.Abs.EVar ident -> failure x
  Latte.Abs.ELitInt integer -> failure x
  Latte.Abs.ELitTrue -> failure x
  Latte.Abs.ELitFalse -> failure x
  Latte.Abs.EApp ident exprs -> failure x
  Latte.Abs.EString string -> failure x
  Latte.Abs.Neg expr -> failure x
  Latte.Abs.Not expr -> failure x
  Latte.Abs.EMul expr1 mulop expr2 -> failure x
  Latte.Abs.EAdd expr1 addop expr2 -> failure x
  Latte.Abs.ERel expr1 relop expr2 -> failure x
  Latte.Abs.EAnd expr1 expr2 -> failure x
  Latte.Abs.EOr expr1 expr2 -> failure x

transAddOp :: Latte.Abs.AddOp -> Result
transAddOp x = case x of
  Latte.Abs.Plus -> failure x
  Latte.Abs.Minus -> failure x

transMulOp :: Latte.Abs.MulOp -> Result
transMulOp x = case x of
  Latte.Abs.Times -> failure x
  Latte.Abs.Div -> failure x
  Latte.Abs.Mod -> failure x

transRelOp :: Latte.Abs.RelOp -> Result
transRelOp x = case x of
  Latte.Abs.LTH -> failure x
  Latte.Abs.LE -> failure x
  Latte.Abs.GTH -> failure x
  Latte.Abs.GE -> failure x
  Latte.Abs.EQU -> failure x
  Latte.Abs.NE -> failure x

module Compiler where

import Control.Monad.Except
import Control.Monad.State
import Data.Char (ord)
import Data.Map as Map
import Env
import GHC.RTS.Flags (TraceFlags (user))
import Instructions
import Latte.Abs
import Numeric
import Types

type LLVMCode = String

type StrDeclarations = String

type ArgsCode = String

type InitArgsCode = String

type ExprResult = (Register, LLVMCode, CType, StrDeclarations)

pref :: String -> String
pref str
  | length str > 1 = str
  | otherwise = "0" ++ str

prepString :: [Char] -> [Char]
prepString (c : str) = "\\" ++ pref (showHex (ord c) "") ++ prepString str
prepString [] = ""

funcDeclarations :: String
funcDeclarations =
  "declare i32 @puts(i8*)\n"
    ++ "declare i8* @readString()\n"
    ++ "declare i32 @readInt()\n"
    ++ "declare i8* @malloc(i32)\n"
    ++ "declare i8* @strcat(i8*,i8*)\n"
    ++ "declare i32 @strlen(i8*)\n"
    ++ "declare i8* @strcpy(i8*,i8*)\n"
    ++ "declare void @printInt(i32 %x)\n"
    ++ "declare void @printString(i8* %x)\n"
    ++ "declare i32 @printf(i8*, ...)\n"
    ++ "declare i32 @scanf(i8*, ...)\n"
    ++ "declare void @error()\n"
    ++ "declare i8* @concat(i8* %s1, i8* %s2)\n"

compile :: Program -> IO (Either Error String)
compile program = do
  result <- runStateT (runExceptT (compileProgram program)) initEnv
  case fst result of
    (Left error) -> return $ Left error
    (Right resultText) -> return $ Right (funcDeclarations ++ resultText)

compileProgram :: Program -> Compl LLVMCode
compileProgram (Program pos topDefs) = do
  addFuncDefinition topDefs
  compileFuncDefs topDefs

addFuncDefinition :: [TopDef] -> Compl ()
addFuncDefinition [] = return ()
addFuncDefinition ((FnDef pos retType (Ident name) args block) : defs) = do
  _ <- addProc (getCType retType) (Ident name) (Prelude.map getArgCType args)
  addFuncDefinition defs

compileFuncDefs :: [TopDef] -> Compl LLVMCode
compileFuncDefs [] = return ""
compileFuncDefs (def : defs) = do
  llvmCode1 <- compileFuncDef def
  llvmCode2 <- compileFuncDefs defs
  return $ llvmCode1 ++ llvmCode2

getArgCType :: Arg -> CType
getArgCType (Arg pos argType ident) = getCType argType

compileFuncDef :: TopDef -> Compl LLVMCode
compileFuncDef (FnDef pos retType (Ident name) args block) = do
  (argsCode, initArgsCode) <- compileArgs args
  (blockCode, strDeclarations) <- compileBlock block
  let blockCore = strDeclarations ++ "\ndefine " ++ typeToLLVM retType ++ " @" ++ name ++ "(" ++ argsCode ++ ") {\n" ++ initArgsCode ++ blockCode
  case retType of
    Void _ -> return $ blockCore ++ "ret void\n}\n"
    Str _ -> return $ blockCore ++ "%_ = call i8* @malloc(i32 1)\n ret i8* %_\n\n}\n"
    _ -> return $ blockCore ++ "ret " ++ typeToLLVM retType ++ " 0\n}\n"

compileArgs :: [Arg] -> Compl (ArgsCode, InitArgsCode)
compileArgs [] = do return ("", "")
compileArgs [Arg pos argType ident] = do
  reg <- useNewReg
  (initCode, _) <- initVar (getCType argType) [NoInit pos ident]
  var <- lastVar
  return (show (getCType argType) ++ " " ++ show reg, initCode ++ show (SetV var (getCType argType) reg))
compileArgs (arg : args) = do
  (argCode, initArgCode) <- compileArgs [arg]
  (argsCode, initArgsCode) <- compileArgs args
  return (argCode ++ "," ++ argsCode, initArgCode ++ initArgsCode)

compileBlock :: Block -> Compl (LLVMCode, StrDeclarations)
compileBlock (Block pos stmts) = compileStmts stmts

compileStmts :: [Stmt] -> Compl (LLVMCode, StrDeclarations)
compileStmts [] = do return ("", "")
compileStmts (stmt : stmts) = do
  (stmtCode, strDeclarations1) <- compileStmt stmt
  (stmtsCode, strDeclarations2) <- compileStmts stmts
  return (stmtCode ++ stmtsCode, strDeclarations1 ++ strDeclarations2)

compileStmt :: Stmt -> Compl (LLVMCode, StrDeclarations)
compileStmt (Empty pos) = do return ("", "")
compileStmt (BStmt _ (Block _ stmts)) = do
  (penv, venv, store, loc, reg, label, var) <- get
  (blockCode, strDeclarations) <- compileStmts stmts
  (postPenv, postVenv, postStore, postLoc, postReg, postLabel, postVar) <- get
  put (penv, venv, postStore, loc, postReg, postLabel, postVar)
  return ("\n " ++ blockCode ++ "\n", strDeclarations)
compileStmt (Decl pos varType items) = initVar (getCType varType) items
compileStmt (Ass pos ident expr) = do
  (exprReg, exprCode, _, strDeclarations) <- compileExpr expr
  (varType, var) <- getVar ident
  return (exprCode ++ show (SetV var varType exprReg), strDeclarations)
compileStmt (Incr pos ident) = compileStmt (Ass pos ident (EAdd pos (EVar pos ident) (Plus pos) (ELitInt pos 1)))
compileStmt (Decr pos ident) = compileStmt (Ass pos ident (EAdd pos (EVar pos ident) (Minus pos) (ELitInt pos 1)))
compileStmt (Ret pos expr) = do
  (exprReg, exprCode, exprType, strDeclarations) <- compileExpr expr
  return (exprCode ++ show (RetI exprType exprReg), strDeclarations)
compileStmt (VRet _) = return (show VRetI, "")
compileStmt (Cond _ (ELitTrue _) stmt) = compileStmt stmt
compileStmt (Cond _ (ELitFalse _) stmt) = return ("", "")
compileStmt (Cond _ expr stmt) = do
  (exprReg, exprCode, exprType, strDeclarations1) <- compileExpr expr
  (stmtCode, strDeclarations2) <- compileStmt stmt
  labTrue <- useLabel
  labFalse <- useLabel
  labEnd <- useLabel
  return (exprCode ++ show (IfElseI exprReg labTrue labFalse labEnd stmtCode ""), strDeclarations1 ++ strDeclarations2)
compileStmt (CondElse _ (ELitTrue _) stmt1 stmt2) = compileStmt stmt1
compileStmt (CondElse _ (ELitFalse _) stmt1 stmt2) = compileStmt stmt2
compileStmt (CondElse _ expr stmt1 stmt2) = do
  (exprReg, exprCode, exprType, strDeclarations1) <- compileExpr expr
  (stmt1Res, strDeclarations2) <- compileStmt stmt1
  (stmt2Res, strDeclarations3) <- compileStmt stmt2
  labTrue <- useLabel
  labFalse <- useLabel
  labEnd <- useLabel
  return (exprCode ++ show (IfElseI exprReg labTrue labFalse labEnd stmt1Res stmt2Res), strDeclarations1 ++ strDeclarations2 ++ strDeclarations3)
compileStmt (While pos expr stmt) = do
  (exprReg, exprCode, exprType, strDeclarations1) <- compileExpr expr
  (stmtRes, strDeclarations2) <- compileStmt stmt
  labCheck <- useLabel
  labTrue <- useLabel
  labEnd <- useLabel
  return (show (WhileI exprReg exprCode labCheck labTrue labEnd stmtRes), strDeclarations1 ++ strDeclarations2)
compileStmt (SExp pos expr) = do
  (_, code, _, strDeclarations) <- compileExpr expr
  return (code, strDeclarations)

initVar :: CType -> [Item] -> Compl (LLVMCode, StrDeclarations)
initVar varType [] = do return ("", "")
initVar varType ((NoInit pos ident) : items) = do
  newVar <- addVar varType ident
  (varCode, strDeclarations) <- initVar varType items
  let declCode = show (AddV newVar varType)
  case varType of
    CStr -> return (varCode ++ declCode, strDeclarations)
    _ -> return (varCode ++ declCode ++ show (InitI newVar varType), strDeclarations)
initVar varType ((Init pos ident expr) : items) = do
  (exprReg, exprCode, _, strDeclarations1) <- compileExpr expr
  newVar <- addVar varType ident
  let initCode = exprCode ++ show (SetV newVar varType exprReg)
  (varsCode, strDeclarations2) <- initVar varType items
  let declCode = show (AddV newVar varType)
  case varType of
    CStr -> return (varsCode ++ declCode ++ initCode, strDeclarations1 ++ strDeclarations2)
    _ -> return (varsCode ++ declCode ++ show (InitI newVar varType) ++ initCode, strDeclarations1 ++ strDeclarations2)

compileExpr :: Expr -> Compl ExprResult
compileExpr (EAdd pos e1 (Plus posOp) e2) = compileBinExp e1 e2 AddOp
compileExpr (EAdd pos e1 (Minus posOp) e2) = compileBinExp e1 e2 SubOp
compileExpr (EMul pos e1 (Times posOp) e2) = compileBinExp e1 e2 MulOp
compileExpr (EMul pos e1 (Div posOp) e2) = compileBinExp e1 e2 DivOp
compileExpr (EMul pos e1 (Mod posOp) e2) = compileBinExp e1 e2 ModOp
compileExpr (ERel pos e1 op e2) = compileCmpExpr e1 e2 op
compileExpr (ELitTrue pos) = do
  reg <- useNewReg
  return (reg, show reg ++ " = " ++ "or i1 1,1" ++ "\n", CBool, "")
compileExpr (ELitFalse pos) = do
  reg <- useNewReg
  return (reg, show reg ++ " = " ++ "or i1 0,0" ++ "\n", CBool, "")
compileExpr (ELitInt pos num) = do
  reg <- useNewReg
  return (reg, show reg ++ " = " ++ "or" ++ " i32 " ++ "0," ++ show num ++ "\n", CInt, "")
compileExpr (EVar pos ident) = do
  resultReg <- useNewReg
  (varType, var) <- getVar ident
  case varType of
    CVoid -> return (Reg 0, "", CVoid, "")
    _ -> return (resultReg, show (GetV var varType resultReg), varType, "")
compileExpr (EApp pos (Ident name) exprs) = do
  (argStr, compileStr, strDeclarations) <- compileArgsExpr exprs
  (retType, argsTypes) <- getProc $ Ident name
  reg <- useNewReg
  case retType of
    CVoid -> return (reg, compileStr ++ "call void @" ++ name ++ "(" ++ argStr ++ ")\n", CVoid, strDeclarations)
    _ -> return (reg, compileStr ++ show reg ++ " = call " ++ show retType ++ " @" ++ name ++ "(" ++ argStr ++ ")\n", retType, strDeclarations)
compileExpr (EString pos str) = do
  reg <- useNewReg
  let (Reg num) = reg
  let len = length str + 1
  let asignCode = show reg ++ " = bitcast [" ++ show len ++ " x i8]* @s" ++ show num ++ " to i8*\n"
  let strDeclarations = "@s" ++ show num ++ " = private constant [" ++ show len ++ " x i8] c\"" ++ prepString str ++ "\\00\"\n"
  return (reg, asignCode, CStr, strDeclarations)
compileExpr (Neg pos expr) = compileExpr (EAdd pos (ELitInt pos 0) (Minus pos) expr)
compileExpr (EAnd pos e1 e2) = do
  -- TODO: Simplify
  (reg1, text1, ctype1, _) <- compileExpr e1
  labE1True <- useLabel
  labE1False <- useLabel
  labEnd <- useLabel
  (Reg num) <- useNewReg
  let ident = Ident $ "and" ++ show num
  (varText, sd) <- initVar CBool [Init pos ident (ELitTrue pos)]
  (setTrueText, sd2) <- compileStmt (Ass pos ident e2)
  (setE2Text, sd3) <- compileStmt (Ass pos ident (ELitFalse pos))
  let ifInstr = IfElseI reg1 labE1True labE1False labEnd setTrueText setE2Text
  (ctype, var) <- getVar ident
  res <- useNewReg
  return (res, varText ++ text1 ++ show ifInstr ++ show (GetV var ctype res), CBool, sd ++ sd2 ++ sd3)
compileExpr (EOr pos e1 e2) = do
  -- TODO: Simplify
  (reg1, text1, ctype1, _) <- compileExpr e1
  labE1True <- useLabel
  labE1False <- useLabel
  labEnd <- useLabel
  (Reg num) <- useNewReg
  let ident = Ident $ "or" ++ show num
  (varText, sd1) <- initVar CBool [Init pos ident (ELitFalse pos)]
  (setTrueText, sd2) <- compileStmt (Ass pos ident (ELitTrue pos))
  (setE2Text, sd3) <- compileStmt (Ass pos ident e2)
  let ifInstr = IfElseI reg1 labE1True labE1False labEnd setTrueText setE2Text
  (ctype, var) <- getVar ident
  res <- useNewReg
  return (res, varText ++ text1 ++ show ifInstr ++ show (GetV var ctype res), CBool, sd1 ++ sd2 ++ sd3)
compileExpr (Not pos expr) = do
  (exprReg, code, ctype, strDeclarations) <- compileExpr expr
  reg <- useNewReg
  return (reg, code ++ show (BoolI reg XorOp (IntVal 1) (RegVal exprReg)), CBool, strDeclarations)

compileArgsExpr :: [Expr] -> Compl (String, String, String)
compileArgsExpr [] = return ("", "", "")
compileArgsExpr [expr] = do
  (reg, exprCode, ctype, strDeclarations) <- compileExpr expr
  return (show ctype ++ " " ++ show reg, exprCode, strDeclarations)
compileArgsExpr (expr : exprs) = do
  (reg, exprCode, ctype, strDeclarations1) <- compileExpr expr
  (argStr, argsCode, strDeclarations2) <- compileArgsExpr exprs
  return (show ctype ++ " " ++ show reg ++ "," ++ argStr, exprCode ++ argsCode, strDeclarations1 ++ strDeclarations2)

compileBinExp :: Expr -> Expr -> ArtOp -> Compl ExprResult
compileBinExp e1 e2 op = do
  (exprReg1, exprCode1, e1Type, strDeclarations1) <- compileExpr e1
  (exprReg2, exprCode2, e2Type, strDeclarations2) <- compileExpr e2
  case e1Type of
    CStr -> do
      (resultReg, resultCode, _, strDeclarations3) <- compileExpr (EApp (Just (0, 0)) (Ident "concat") [e1, e2])
      return (resultReg, resultCode, CStr, strDeclarations1 ++ strDeclarations2 ++ strDeclarations3)
    _ -> do
      reg <- useNewReg
      return (reg, exprCode1 ++ exprCode2 ++ show (ArtI op (RegVal exprReg1) (RegVal exprReg2) reg), e1Type, strDeclarations1 ++ strDeclarations2)

compileCmpExpr :: Expr -> Expr -> RelOp -> Compl ExprResult
compileCmpExpr e1 e2 op = do
  -- TODO: str cmp
  (reg1, code1, t1, strDeclarations1) <- compileExpr e1
  (reg2, code2, t2, strDeclarations2) <- compileExpr e2
  reg <- useNewReg
  return (reg, code1 ++ code2 ++ show (CmpI op t1 (RegVal reg1) (RegVal reg2) reg), CBool, strDeclarations1 ++ strDeclarations2)
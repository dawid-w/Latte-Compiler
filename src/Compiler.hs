module Compiler where

import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map
import Env
import Instructions
import Latte.Abs
import Types

type Result = String

type ExprResult = (Register, Result, CType)

indent :: String -> String
indent str = "    " ++ indentH str

indentH :: [Char] -> [Char]
indentH ('\n' : str) = "\n    " ++ indentH str
indentH (c : str) = c : indentH str
indentH [] = ""

compile :: Program -> IO (Either Error String)
compile program = do
  result <- runStateT (runExceptT (compileProgram program)) initEnv
  case fst result of
    (Left error) -> return $ Left error
    (Right resultText) ->
      return $
        Right
          ( "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\n"
              ++ "declare i32 @printf(i8*, ...)\n"
              ++ "define void @printInt(i32 %x) {\n"
              ++ "    %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n"
              ++ "    call i32 (i8*, ...) @printf(i8* %t0, i32 %x) \n"
              ++ "    ret void\n"
              ++ "}\n"
              ++ resultText
              ++ "\n"
          )

compileProgram :: Program -> Compl Result
compileProgram (Program pos topDefs) = compileDefs topDefs

compileDefs :: [TopDef] -> Compl Result
compileDefs [] = return ""
compileDefs (def : defs) = do
  resultText1 <- compileDef def
  resultText2 <- compileDefs defs
  return $ resultText1 ++ resultText2

getArgCType :: Arg -> CType
getArgCType (Arg pos argType ident) = getCType argType

compileDef :: TopDef -> Compl Result
compileDef (FnDef pos retType (Ident name) args block) = do
  _ <- addProc (getCType retType) (Ident name) (Prelude.map getArgCType args)
  argsStr <- defArgs args
  blockStr <- compileBlock block
  return $ "define " ++ typeToLLVM retType ++ " @" ++ name ++ "(" ++ argsStr ++ ") {\n" ++ indent blockStr ++ "\n}\n"

defArgs :: [Arg] -> Compl Result
defArgs [] = do return ""
defArgs [Arg pos argType ident] = do
  reg <- addVar (getCType argType) ident
  return $ typeToLLVM argType ++ " " ++ show reg
defArgs ((Arg pos argType ident) : args) = do
  argsStr <- defArgs args
  reg <- addVar (getCType argType) ident
  return $ argsStr ++ "," ++ typeToLLVM argType ++ " " ++ show reg

compileBlock :: Block -> Compl Result
compileBlock (Block pos stmts) = compileStmts stmts

compileStmts :: [Stmt] -> Compl Result
compileStmts [] = do return ""
compileStmts (stmt : stmts) = do
  r1 <- compileStmt stmt
  r2 <- compileStmts stmts
  return $ r1 ++ r2

compileStmt :: Stmt -> Compl Result
compileStmt (BStmt pos block) = do
  (penv, venv, store, loc, reg) <- get
  let (Block pos stmts) = block
  blockText <- compileStmts stmts
  (postPenv, postVenv, postStore, postLoc, postReg) <- get
  put (penv, venv, postStore, loc, postReg)
  return $ "\n " ++ indent blockText ++ "\n\n"
compileStmt (Decl pos varType items) = do
  initVar (getCType varType) items
compileStmt (Ret pos expr) = do
  (reg, text, exprType) <- compileExpr expr
  return $ text ++ "ret " ++ show exprType ++ " " ++ show reg
compileStmt (Ass pos ident expr) = do
  (exprReg, exprText, exprType) <- compileExpr expr
  (varType, _) <- getVar ident
  varReg <- setVar varType ident
  return $ exprText ++ show varReg ++ " = or " ++ show exprType ++ " 0, " ++ show exprReg ++ "\n"
compileStmt (VRet pos) = return ""
compileStmt (SExp pos expr) = do
  (reg, text, retType) <- compileExpr expr
  return text
compileStmt _ = do return ""

initVar :: CType -> [Item] -> Compl Result
initVar varType [] = do return ""
initVar varType ((NoInit pos ident) : items) = do
  addVar varType ident
  initVar varType items
initVar varType ((Init pos ident expr) : items) = do
  addVar varType ident
  r1 <- compileStmt (Ass pos ident expr)
  r2 <- initVar varType items
  return $ r1 ++ r2

compileExpr :: Expr -> Compl ExprResult
compileExpr (EAdd pos e1 (Plus posOp) e2) = compileBinExp e1 e2 AddOp
compileExpr (EAdd pos e1 (Minus posOp) e2) = compileBinExp e1 e2 SubOp
compileExpr (EMul pos e1 (Times posOp) e2) = compileBinExp e1 e2 AddOp
compileExpr (EMul pos e1 (Div posOp) e2) = compileBinExp e1 e2 AddOp
compileExpr (EMul pos e1 (Mod posOp) e2) = compileBinExp e1 e2 AddOp
compileExpr (ELitTrue pos) = do
  reg <- useReg
  return (reg, show reg ++ " = " ++ "or i1 1,1" ++ "\n", CBool)
compileExpr (ELitFalse pos) = do
  reg <- useReg
  return (reg, show reg ++ " = " ++ "or i1 0,0" ++ "\n", CBool)
compileExpr (ELitInt pos num) = do
  reg <- useReg
  return (reg, show reg ++ " = " ++ "or" ++ " i32 " ++ "0," ++ show num ++ "\n", CInt)
compileExpr (EVar pos ident) = do
  reg <- useReg
  (varType, varReg) <- getVar ident
  case varType of
    CInt -> return (reg, show reg ++ " = or i32 0, " ++ show varReg ++ "\n", CInt)
    CBool -> return (reg, show reg ++ " = or i1 0, " ++ show varReg ++ "\n", CBool)
    CVoid -> return (Reg 0, "", CVoid)
    _ -> return (Reg 0, "", CVoid)
compileExpr (EApp pos (Ident name) exprs) = do
  (argStr, compileStr) <- compileArgsExpr exprs
  (retType, argsTypes) <- getProc $ Ident name
  case retType of
    CVoid -> do return (Reg 0, compileStr ++ "call void @" ++ name ++ "(" ++ argStr ++ ")\n", CInt)
    _ -> do
      reg <- useReg
      return (reg, compileStr ++ show reg ++ " = call " ++ show retType ++ " @" ++ name ++ "(" ++ argStr ++ ")\n", CInt)
compileExpr (EString pos str) = do return (Reg 0, "", CStr)
compileExpr _ = do return (Reg 0, "", CVoid)

compileArgsExpr :: [Expr] -> Compl (String, String)
compileArgsExpr [] = return ("", "")
compileArgsExpr [expr] = do
  (reg, text, ctype) <- compileExpr expr
  return (show ctype ++ " " ++ show reg, text)
compileArgsExpr (expr : exprs) = do
  (reg, text, ctype) <- compileExpr expr
  (argStr, compileStr) <- compileArgsExpr exprs
  return (show ctype ++ " " ++ show reg ++ "," ++ argStr, text ++ compileStr)

compileBinExp :: Expr -> Expr -> ArtOp -> Compl ExprResult
compileBinExp e1 e2 op = do
  (reg1, result1, t1) <- compileExpr e1
  (reg2, result2, t2) <- compileExpr e2
  reg <- useReg
  return (reg, result1 ++ result2 ++ show (ArtI op (RegVal reg1) (RegVal reg2) reg), t1)

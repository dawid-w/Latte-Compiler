module Compiler where

import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map
import Env
import Instructions
import Latte.Abs
import Types

type Result = String

type StrDecl = String

type ExprResult = (Register, Result, CType, StrDecl)

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
          ( "declare i32 @puts(i8*)\n"
              ++ "declare i8* @malloc(i32)\n"
              ++ "declare i8* @strcat(i8*,i8*)\n"
              ++ "declare i32 @strlen(i8*)\n"
              ++ "declare i8* @strcpy(i8*,i8*)\n"
              ++ "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\n"
              ++ "declare i32 @printf(i8*, ...)\n"
              ++ "define void @printInt(i32 %x) {\n"
              ++ "    %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n"
              ++ "    call i32 (i8*, ...) @printf(i8* %t0, i32 %x) \n"
              ++ "    ret void\n"
              ++ "}\n"
              ++ "define i8* @concat(i8* %s1, i8* %s2) {\n"
              ++ "%1 = call i32 @strlen(i8* %s1)\n"
              ++ "%2 = call i32 @strlen(i8* %s2)\n"
              ++ "%3 = add i32 %1, 1\n"
              ++ "%4 = add i32 %3, %2\n"
              ++ "%5 = call i8* @malloc(i32 %4)\n"
              ++ "%6 = call i8* @strcpy(i8* %5, i8* %s1)\n"
              ++ "%7 = call i8* @strcat(i8* %6, i8* %s2)\n"
              ++ "ret i8* %7 \n"
              ++ "}\n\n"
              ++ "define void @printString(i8* %s3){\n"
              ++ "%1 = call i32 @puts(i8* %s3)\n"
              ++ "ret void\n"
              ++ "}\n\n"
              ++ resultText
              ++ "\n"
          )

compileProgram :: Program -> Compl Result
compileProgram (Program pos topDefs) = do
  addDefs topDefs
  compileDefs topDefs

addDefs :: [TopDef] -> Compl ()
addDefs [] = return ()
addDefs ((FnDef pos retType (Ident name) args block) : defs) = do
  _ <- addProc (getCType retType) (Ident name) (Prelude.map getArgCType args)
  addDefs defs

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
  (argsStr, initStr) <- defArgs args
  (blockStr, strDecl) <- compileBlock block
  case retType of
    Void ma -> return $ strDecl ++ "\ndefine " ++ typeToLLVM retType ++ " @" ++ name ++ "(" ++ argsStr ++ ") {\n" ++ initStr ++ indent blockStr  ++ "ret void\n}\n"
    _ -> return $ strDecl ++ "\ndefine " ++ typeToLLVM retType ++ " @" ++ name ++ "(" ++ argsStr ++ ") {\n" ++ initStr ++ indent blockStr ++ "\n}\n"

  

defArgs :: [Arg] -> Compl (String, String)
defArgs [] = do return ("", "")
defArgs [Arg pos argType ident] = do
  reg <- useReg
  (initRes, _) <- initVar (getCType argType) [NoInit pos ident]
  var <- lastVar
  return (show (getCType argType) ++ " " ++ show reg, initRes ++ show (SetV var (getCType argType) reg))
defArgs (arg : args) = do
  (argsStr, initStr) <- defArgs [arg]
  (argsStrs, initStrs) <- defArgs args
  return (argsStr ++ "," ++ argsStrs, initStr ++ initStrs)

compileBlock :: Block -> Compl (Result, String)
compileBlock (Block pos stmts) = compileStmts stmts

compileStmts :: [Stmt] -> Compl (Result, String)
compileStmts [] = do return ("", "")
compileStmts (stmt : stmts) = do
  (r1, s1) <- compileStmt stmt
  (r2, s2) <- compileStmts stmts
  return (r1 ++ r2, s1 ++ s2)

compileStmt :: Stmt -> Compl (Result, String)
compileStmt (Empty pos) = do return ("", "")
compileStmt (BStmt pos block) = do
  (penv, venv, store, loc, reg, label, var) <- get
  let (Block pos stmts) = block
  (blockText, strDecl) <- compileStmts stmts
  (postPenv, postVenv, postStore, postLoc, postReg, postLabel, postVar) <- get
  put (penv, venv, postStore, loc, postReg, postLabel, postVar)
  return ("\n " ++ indent blockText ++ "\n\n", strDecl)
compileStmt (Decl pos varType items) = do
  (r, s) <- initVar (getCType varType) items
  return (r, s)
compileStmt (Ass pos ident expr) = do
  (exprReg, exprText, exprType, declStr) <- compileExpr expr
  (varType, var) <- getVar ident
  return (exprText ++ show (SetV var varType exprReg), declStr)
compileStmt (Incr pos ident) = do
  compileStmt (Ass pos ident (EAdd pos (EVar pos ident) (Plus pos) (ELitInt pos 1)))
compileStmt (Decr pos ident) = do
  compileStmt (Ass pos ident (EAdd pos (EVar pos ident) (Minus pos) (ELitInt pos 1)))
compileStmt (Ret pos expr) = do
  (reg, text, exprType, strDecl) <- compileExpr expr
  return (text ++ "ret " ++ show exprType ++ " " ++ show reg ++ "\n", strDecl)
compileStmt (VRet pos) = return ("ret void\n", "")
compileStmt (Cond pos (ELitTrue _) stmt) = do
  compileStmt stmt
compileStmt (Cond pos (ELitFalse _) stmt) = return ("", "")
compileStmt (Cond pos expr stmt) = do
  (exprReg, exprText, exprType, _) <- compileExpr expr
  (stmtRes, _) <- compileStmt stmt
  labTrue <- useLabel
  labFalse <- useLabel
  labEnd <- useLabel
  return (exprText ++ show (IfElseI exprReg labTrue labFalse labEnd stmtRes ""), "")
compileStmt (CondElse pos (ELitTrue _) stmt1 stmt2) = compileStmt stmt1
compileStmt (CondElse pos (ELitFalse _) stmt1 stmt2) = compileStmt stmt2
compileStmt (CondElse pos expr stmt1 stmt2) = do
  (exprReg, exprText, exprType, _) <- compileExpr expr
  (stmt1Res, _) <- compileStmt stmt1
  (stmt2Res, _) <- compileStmt stmt2
  labTrue <- useLabel
  labFalse <- useLabel
  labEnd <- useLabel
  return (exprText ++ show (IfElseI exprReg labTrue labFalse labEnd stmt1Res stmt2Res), "")
compileStmt (While pos expr stmt) = do
  (exprReg, exprText, exprType, _) <- compileExpr expr
  (stmtRes, _) <- compileStmt stmt
  labCheck <- useLabel
  labTrue <- useLabel
  labEnd <- useLabel
  return (show (WhileI exprReg exprText labCheck labTrue labEnd stmtRes), "")
compileStmt (SExp pos expr) = do
  (reg, text, retType, strDecl) <- compileExpr expr
  return (text, strDecl)

initVar :: CType -> [Item] -> Compl (Result, StrDecl)
initVar varType [] = do return ("", "")
initVar varType ((NoInit pos ident) : items) = do
  newVar <- addVar varType ident
  case varType of
    CStr -> do
      (rs, strDecl) <- initVar varType items
      return (rs ++ show (AddV newVar varType), strDecl)
    _ -> do
      (rs, strDecl) <- initVar varType items
      return (rs ++ show (AddInit newVar varType), strDecl)
initVar varType ((Init pos ident expr) : items) = do
  case varType of
    CStr -> do
      newVar <- addVar varType ident
      (rs, ds1) <- initVar varType items
      (r1, sExpr) <- compileStmt (Ass pos ident expr)
      (r2, ds2) <- initVar varType items
      return (rs ++ show (AddV newVar varType) ++ r1 ++ r2, ds1 ++ ds2 ++ sExpr)
    _ -> do
      newVar <- addVar varType ident
      (rs, ds1) <- initVar varType items
      (r1, sExpr) <- compileStmt (Ass pos ident expr)
      (r2, ds2) <- initVar varType items
      return (rs ++ show (AddInit newVar varType) ++ r1 ++ r2, ds1 ++ ds2 ++ sExpr)

compileExpr :: Expr -> Compl ExprResult
compileExpr (EAdd pos e1 (Plus posOp) e2) = compileBinExp e1 e2 AddOp
compileExpr (EAdd pos e1 (Minus posOp) e2) = compileBinExp e1 e2 SubOp
compileExpr (EMul pos e1 (Times posOp) e2) = compileBinExp e1 e2 AddOp
compileExpr (EMul pos e1 (Div posOp) e2) = compileBinExp e1 e2 AddOp
compileExpr (EMul pos e1 (Mod posOp) e2) = compileBinExp e1 e2 AddOp
compileExpr (ERel pos e1 op e2) = compileCmpExpr e1 e2 op
compileExpr (ELitTrue pos) = do
  reg <- useReg
  return (reg, show reg ++ " = " ++ "or i1 1,1" ++ "\n", CBool, "")
compileExpr (ELitFalse pos) = do
  reg <- useReg
  return (reg, show reg ++ " = " ++ "or i1 0,0" ++ "\n", CBool, "")
compileExpr (ELitInt pos num) = do
  reg <- useReg
  return (reg, show reg ++ " = " ++ "or" ++ " i32 " ++ "0," ++ show num ++ "\n", CInt, "")
compileExpr (EVar pos ident) = do
  varReg <- useReg
  resReg <- useReg
  (varType, var) <- getVar ident
  case varType of
    CInt -> return (resReg, show (GetV var varType varReg) ++ show resReg ++ " = or i32 0, " ++ show varReg ++ "\n", CInt, "")
    CBool -> return (resReg, show (GetV var varType varReg) ++ show resReg ++ " = or i1 0, " ++ show varReg ++ "\n", CBool, "")
    CStr -> return (varReg, show (GetV var varType varReg), CStr, "")
    _ -> return (Reg 0, "", CVoid, "")
compileExpr (EApp pos (Ident name) exprs) = do
  (argStr, compileStr, ds) <- compileArgsExpr exprs
  (retType, argsTypes) <- getProc $ Ident name
  case retType of
    CVoid -> do return (Reg 0, compileStr ++ "call void @" ++ name ++ "(" ++ argStr ++ ")\n", CInt, ds)
    _ -> do
      reg <- useReg
      return (reg, compileStr ++ show reg ++ " = call " ++ show retType ++ " @" ++ name ++ "(" ++ argStr ++ ")\n", CInt, ds)
compileExpr (EString pos str) = do
  reg <- useReg
  let (Reg num) = reg
  let len = length str + 1
  let as = show reg ++ " = bitcast [" ++ show len ++ " x i8]* @s" ++ show num ++ " to i8*\n"
  let ds = "@s" ++ show num ++ " = private constant [" ++ show len ++ " x i8] c\"" ++ str ++ "\\00\"\n"
  return (reg, as, CStr, ds)
compileExpr (Neg pos expr) = compileExpr (EAdd pos (ELitInt pos 0) (Minus pos) expr)
compileExpr (EAnd pos e1 e2) = do
  (reg1, text1, ctype1, _) <- compileExpr e1
  (reg2, text2, ctype2, _) <- compileExpr e2
  reg <- useReg
  return (reg, text1 ++ text2 ++ show (BoolI reg AndOp (RegVal reg1) (RegVal reg2)), CBool, "")
compileExpr (EOr pos e1 e2) = do
  (reg1, text1, ctype1, _) <- compileExpr e1
  (reg2, text2, ctype2, _) <- compileExpr e2
  reg <- useReg
  return (reg, text1 ++ text2 ++ show (BoolI reg OrOp (RegVal reg1) (RegVal reg2)), CBool, "")
compileExpr (Not pos expr) = do
  (exprReg, text, ctype, _) <- compileExpr expr
  reg <- useReg
  return (reg, text ++ show (BoolI reg XorOp (IntVal 1) (RegVal exprReg)), CBool, "")

compileArgsExpr :: [Expr] -> Compl (String, String, String)
compileArgsExpr [] = return ("", "", "")
compileArgsExpr [expr] = do
  (reg, text, ctype, ds) <- compileExpr expr
  return (show ctype ++ " " ++ show reg, text, ds)
compileArgsExpr (expr : exprs) = do
  (reg, text, ctype, ds) <- compileExpr expr
  (argStr, compileStr, dss) <- compileArgsExpr exprs
  return (show ctype ++ " " ++ show reg ++ "," ++ argStr, text ++ compileStr, ds ++ dss)

compileBinExp :: Expr -> Expr -> ArtOp -> Compl ExprResult
compileBinExp e1 e2 op = do
  (reg1, result1, t1, _) <- compileExpr e1
  case t1 of
    CStr -> do
      (rr, resr, tr, concatDeclStr) <- compileExpr (EApp (Just (0, 0)) (Ident "concat") [e1, e2])
      return (rr, resr, CStr, concatDeclStr)
    _ -> do
      (reg1, result1, t1, declStr1) <- compileExpr e1
      (reg2, result2, t2, declStr2) <- compileExpr e2
      reg <- useReg
      return (reg, result1 ++ result2 ++ show (ArtI op (RegVal reg1) (RegVal reg2) reg), t1, declStr1 ++ declStr2)

compileCmpExpr :: Expr -> Expr -> RelOp -> Compl ExprResult
compileCmpExpr e1 e2 op = do
  -- TODO: str cmp
  (reg1, result1, t1, _) <- compileExpr e1
  (reg2, result2, t2, _) <- compileExpr e2
  reg <- useReg
  return (reg, result1 ++ result2 ++ show (CmpI op t1 (RegVal reg1) (RegVal reg2) reg), CBool, "")
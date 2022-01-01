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
  (argsStr, initStr) <- defArgs args
  blockStr <- compileBlock block
  return $ "define " ++ typeToLLVM retType ++ " @" ++ name ++ "(" ++ argsStr ++ ") {\n" ++ initStr ++ indent blockStr ++ "\n}\n"

defArgs :: [Arg] -> Compl (String, String)
defArgs [] = do return ("", "")
defArgs [Arg pos argType ident] = do
  reg <- useReg
  initRes <- initVar (getCType argType) [NoInit pos ident]
  var <- lastVar
  return (show (getCType argType) ++ " " ++ show reg, initRes ++ show (SetV var (getCType argType) reg))
defArgs ((Arg pos argType ident) : args) = do
  (argsStr, initStr) <- defArgs args
  reg <- useReg
  initRes <- initVar (getCType argType) [NoInit pos ident]
  var <- lastVar
  return (argsStr ++ ", " ++ show (getCType argType) ++ " " ++ show reg, initStr ++ initRes ++ show (SetV var (getCType argType) reg))

compileBlock :: Block -> Compl Result
compileBlock (Block pos stmts) = compileStmts stmts

compileStmts :: [Stmt] -> Compl Result
compileStmts [] = do return ""
compileStmts (stmt : stmts) = do
  r1 <- compileStmt stmt
  r2 <- compileStmts stmts
  return $ r1 ++ r2

compileStmt :: Stmt -> Compl Result
compileStmt (Empty pos) = do return ""
compileStmt (BStmt pos block) = do
  (penv, venv, store, loc, reg, label, var) <- get
  let (Block pos stmts) = block
  blockText <- compileStmts stmts
  (postPenv, postVenv, postStore, postLoc, postReg, postLabel, postVar) <- get
  put (penv, venv, postStore, loc, postReg, postLabel, postVar)
  return $ "\n " ++ indent blockText ++ "\n\n"
compileStmt (Decl pos varType items) = do
  initVar (getCType varType) items
compileStmt (Ass pos ident expr) = do
  (exprReg, exprText, exprType) <- compileExpr expr
  (varType, var) <- getVar ident
  return $ exprText ++ show (SetV var varType exprReg)
compileStmt (Incr pos ident) = do
  compileStmt (Ass pos ident (EAdd pos (EVar pos ident) (Plus pos) (ELitInt pos 1)))
compileStmt (Decr pos ident) = do
  compileStmt (Ass pos ident (EAdd pos (EVar pos ident) (Minus pos) (ELitInt pos 1)))
compileStmt (Ret pos expr) = do
  (reg, text, exprType) <- compileExpr expr
  return $ text ++ "ret " ++ show exprType ++ " " ++ show reg
compileStmt (VRet pos) = return ""
compileStmt (Cond pos expr stmt) = do
  (exprReg, exprText, exprType) <- compileExpr expr
  stmtRes <- compileStmt stmt
  labTrue <- useLabel
  labFalse <- useLabel
  labEnd <- useLabel
  return $ exprText ++ show (IfElseI exprReg labTrue labFalse labEnd stmtRes "")
compileStmt (CondElse pos expr stmt1 stmt2) = do
  (exprReg, exprText, exprType) <- compileExpr expr
  stmt1Res <- compileStmt stmt1
  stmt2Res <- compileStmt stmt2
  labTrue <- useLabel
  labFalse <- useLabel
  labEnd <- useLabel
  return $ exprText ++ show (IfElseI exprReg labTrue labFalse labEnd stmt1Res stmt2Res)
compileStmt (While pos expr stmt) = do
  (exprReg, exprText, exprType) <- compileExpr expr
  stmtRes <- compileStmt stmt
  labCheck <- useLabel
  labTrue <- useLabel
  labEnd <- useLabel
  return $ show (WhileI exprReg exprText labCheck labTrue labEnd stmtRes)
compileStmt (SExp pos expr) = do
  (reg, text, retType) <- compileExpr expr
  return text

initVar :: CType -> [Item] -> Compl Result
initVar varType [] = do return ""
initVar varType ((NoInit pos ident) : items) = do
  newVar <- addVar varType ident
  rs <- initVar varType items
  return $ rs ++ show (AddV newVar varType)
initVar varType ((Init pos ident expr) : items) = do
  newVar <- addVar varType ident
  rs <- initVar varType items
  r1 <- compileStmt (Ass pos ident expr)
  r2 <- initVar varType items
  return $ rs ++ show (AddV newVar varType) ++ r1 ++ r2

compileExpr :: Expr -> Compl ExprResult
compileExpr (EAdd pos e1 (Plus posOp) e2) = compileBinExp e1 e2 AddOp
compileExpr (EAdd pos e1 (Minus posOp) e2) = compileBinExp e1 e2 SubOp
compileExpr (EMul pos e1 (Times posOp) e2) = compileBinExp e1 e2 AddOp
compileExpr (EMul pos e1 (Div posOp) e2) = compileBinExp e1 e2 AddOp
compileExpr (EMul pos e1 (Mod posOp) e2) = compileBinExp e1 e2 AddOp
compileExpr (ERel pos e1 op e2) = compileCmpExpr e1 e2 op
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
  varReg <- useReg
  resReg <- useReg
  (varType, var) <- getVar ident
  case varType of
    CInt -> return (resReg, show (GetV var varType varReg) ++ show resReg ++ " = or i32 0, " ++ show varReg ++ "\n", CInt)
    CBool -> return (resReg, show (GetV var varType varReg) ++ show resReg ++ " = or i1 0, " ++ show varReg ++ "\n", CBool)
    _ -> return (Reg 0, "", CVoid)
compileExpr (EApp pos (Ident name) exprs) = do
  (argStr, compileStr) <- compileArgsExpr exprs
  (retType, argsTypes) <- getProc $ Ident name
  case retType of
    CVoid -> do return (Reg 0, compileStr ++ "call void @" ++ name ++ "(" ++ argStr ++ ")\n", CInt)
    _ -> do
      reg <- useReg
      return (reg, compileStr ++ show reg ++ " = call " ++ show retType ++ " @" ++ name ++ "(" ++ argStr ++ ")\n", CInt)
compileExpr (EString pos str) = do return (Reg 0, "", CStr) --TODO
compileExpr (Neg pos expr) = compileExpr (EAdd pos (ELitInt pos 0) (Minus pos) expr)
compileExpr (EAnd pos e1 e2) = do
  (reg1, text1, ctype1) <- compileExpr e1
  (reg2, text2, ctype2) <- compileExpr e2
  reg <- useReg
  return (reg, text1 ++ text2 ++ show (BoolI reg AndOp (RegVal reg1) (RegVal reg2)), CBool)
compileExpr (EOr pos e1 e2) = do
  (reg1, text1, ctype1) <- compileExpr e1
  (reg2, text2, ctype2) <- compileExpr e2
  reg <- useReg
  return (reg, text1 ++ text2 ++ show (BoolI reg OrOp (RegVal reg1) (RegVal reg2)), CBool)
compileExpr (Not pos expr) = do
  (exprReg, text, ctype) <- compileExpr expr
  reg <- useReg
  return (reg, text ++ show (BoolI reg XorOp (IntVal 1) (RegVal exprReg)), CBool)

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

compileCmpExpr :: Expr -> Expr -> RelOp -> Compl ExprResult
compileCmpExpr e1 e2 op = do
  (reg1, result1, t1) <- compileExpr e1
  (reg2, result2, t2) <- compileExpr e2
  reg <- useReg
  return (reg, result1 ++ result2 ++ show (CmpI op t1 (RegVal reg1) (RegVal reg2) reg), CBool)
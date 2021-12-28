module Compiler where

import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map
import Latte.Abs
import Types

type Env = (Map Ident (CType, Int), Int)

type Loc = Int

type Result = String

type RegNum = Int

type Val = Result

type ExprResult = (Int, Result, CType)

type Error = String

type Compl a = ExceptT Error (StateT Env IO) a

initEnv :: Env
initEnv =
  ( fromList
      [ (Ident "printInt", (CFun CVoid [CInt], 0)),
        (Ident "printString", (CFun CVoid [CStr], 0)),
        (Ident "error", (CFun CVoid [], 0)),
        (Ident "readInt", (CFun CInt [], 0)),
        (Ident "readString", (CFun CStr [], 0))
      ],
    0
  )

indent :: [Char] -> [Char]
indent ('\n' : str) = "\n    " ++ indent str
indent (c : str) = c : indent str
indent [] = ""

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

compileProgram :: Program -> Compl Val
compileProgram (Program pos topDefs) = compileDefs topDefs

printError :: Pos -> String -> Compl a
printError (Just (l, c)) text = throwError $ "error at line " ++ show l ++ " column " ++ show c ++ ": " ++ text
printError Nothing text = throwError $ "error: " ++ text

compileDefs :: [TopDef] -> Compl Val
compileDefs [] = return ""
compileDefs (def : defs) = do
  resultText1 <- compileDef def
  resultText2 <- compileDefs defs
  return $ resultText1 ++ resultText2

compileDef :: TopDef -> Compl Val
compileDef (FnDef pos retType (Ident name) args block) = do
  env <- get
  argsStr <- defArgs args
  blockStr <- compileBlock block
  return $ "define " ++ typeToLL retType ++ " @" ++ name ++ "(" ++ argsStr ++ ") {\n    " ++ indent blockStr ++ "\n}\n"

defArgs :: [Arg] -> Compl Val
defArgs [] = do return ""
defArgs [Arg pos argType ident] = do
  (map, reg) <- get
  put (Map.insert ident (getCType argType, reg) map, reg + 1)
  return $ typeToLL argType ++ " %v" ++ show reg
defArgs ((Arg pos argType ident) : args) = do
  argsStr <- defArgs args
  (map, reg) <- get
  put (Map.insert ident (getCType argType, reg) map, reg + 1)
  return $ argsStr ++ "," ++ typeToLL argType ++ " %v" ++ show reg

compileBlock :: Block -> Compl Val
compileBlock (Block pos stmts) = compileStmts stmts

compileStmts :: [Stmt] -> Compl Val
compileStmts [] = do return ""
compileStmts (stmt : stmts) = do
  r1 <- compileStmt stmt
  r2 <- compileStmts stmts
  return $ r1 ++ r2

compileStmt :: Stmt -> Compl Val
compileStmt (Decl pos varType items) = do
  initVar (getCType varType) items
compileStmt (Ret pos expr) = do
  (reg, text, exprType) <- compileExpr expr
  return $ text ++ "ret " ++ show exprType ++ " %v" ++ show reg
compileStmt (Ass pos ident expr) = do
  (reg, text, exprType) <- compileExpr expr
  (map, nextR) <- get
  case Map.lookup ident map of
    (Just (ctype, varReg)) -> do
      let newMap = Map.insert ident (ctype, nextR) map
      put (newMap, nextR + 1)
      return $ text ++ "%v" ++ show nextR ++ " = or " ++ show exprType ++ " 0, %v" ++ show reg ++ "\n"
    Nothing -> do
      let (Ident name) = ident
      throwError $ "Unkfnown ident: " ++ name
compileStmt (Empty pos) = return ""
compileStmt (VRet pos) = return ""
compileStmt _ = do return ""

initVar :: CType -> [Item] -> Compl Val
initVar varType [] = do return ""
initVar varType ((NoInit pos ident) : items) = do
  addVar varType ident
  initVar varType items
initVar varType ((Init pos ident expr) : items) = do
  addVar varType ident
  r1 <- compileStmt (Ass pos ident expr)
  r2 <- initVar varType items
  return $ r1 ++ r2

addVar :: CType -> Ident -> Compl Val
addVar varType ident = do
  (map, nextR) <- get
  let (Ident varName) = ident
  let newMap = Map.insert ident (varType, nextR) map
  put (newMap, nextR + 1)
  return ""

compileExpr :: Expr -> Compl ExprResult
compileExpr (EAdd pos e1 (Plus posOp) e2) = compileBinExp e1 e2 "add"
compileExpr (EAdd pos e1 (Minus posOp) e2) = compileBinExp e1 e2 "sub"
compileExpr (EMul pos e1 (Times posOp) e2) = compileBinExp e1 e2 "mul"
compileExpr (EMul pos e1 (Div posOp) e2) = compileBinExp e1 e2 "div"
compileExpr (EMul pos e1 (Mod posOp) e2) = compileBinExp e1 e2 "mod"
compileExpr (ELitFalse pos) = do
  (map, nextR) <- get
  put (map, nextR + 1)
  return (nextR, "%v" ++ show nextR ++ " = " ++ "or" ++ " i1 " ++ "0,0" ++ "\n", CBool)
compileExpr (ELitInt pos num) = do
  (map, nextR) <- get
  put (map, nextR + 1)
  return (nextR, "%v" ++ show nextR ++ " = " ++ "or" ++ " i32 " ++ "0," ++ show num ++ "\n", CInt)
compileExpr (EVar pos ident) = do
  (map, reg) <- get
  put (map, reg + 1)
  let (Ident name) = ident
  case Map.lookup ident map of
    (Just (CInt, varReg)) -> return (reg, "%v" ++ show reg ++ " = or i32 0, %v" ++ show varReg ++ "\n", CInt)
    (Just (CBool, varReg)) -> return (reg, "%v" ++ show reg ++ " = or i1 0, %v" ++ show varReg ++ "\n", CBool)
    (Just (CVoid, varReg)) -> return (0, "", CVoid)
    (Just (_, varReg)) -> return (0, "", CVoid)
    Nothing -> do
      let (Ident name) = ident
      throwError $ "Unknown ident: " ++ name
compileExpr _ = do return (0, "", CVoid)

compileBinExp :: Expr -> Expr -> String -> Compl ExprResult
compileBinExp e1 e2 s = do
  (reg1, result1, t1) <- compileExpr e1
  (reg2, result2, t2) <- compileExpr e2
  (map, reg) <- get
  put (map, reg + 1)
  return (reg, result1 ++ result2 ++ "%v" ++ show reg ++ " = " ++ s ++ " i32 %v" ++ show reg1 ++ ", %v" ++ show reg2 ++ "\n", t1)

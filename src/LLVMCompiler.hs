module LLVMCompiler where

import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map
import Language.Haskell.TH (varT)
import Latte.Abs
import Text.Parsec.Token (GenLanguageDef (identLetter))
import Types (CType (CBool, CFun, CInt, CStr), Pos, getCType)
import Prelude

type Env = (Map Ident CType)

type Loc = Int

type Result = String

type Val = String

type Error = String

type Compl a = ExceptT Error (StateT Env IO) a

printError :: Pos -> String -> Compl a
printError (Just (l, c)) text = throwError $ "Error at line " ++ show l ++ " column " ++ show c ++ ": " ++ text
printError Nothing text = throwError $ "Error: " ++ text

initEnv :: Env
initEnv = Map.empty

compile :: Program -> IO (Either Error String)
compile program = do
  co <- runStateT (runExceptT (compileProgram program)) initEnv
  case fst co of
    (Left error) -> return $ Left error
    (Right r) ->
      return $ Right ("Program:" ++ r ++ "\n")

compileProgram :: Program -> Compl Val
compileProgram (Program pos topDefs) = do
  _ <- addDefs topDefs
  result <- compileDefs topDefs
  env <- get
  case Map.lookup (Ident "main") env of
    Nothing -> do printError Nothing "No main function"
    Just main -> do return ""

addDefs :: [TopDef] -> Compl Val
addDefs [] = do return ""
addDefs (def : defs) = do
  result <- addDef def
  results <- addDefs defs
  return ""

addDef :: TopDef -> Compl Val
addDef (FnDef pos retType ident args block) = do
  env <- get
  case Map.lookup ident env of
    (Just _) -> printError pos $ "Name" ++ show ident ++ " is already taken."
    Nothing -> do
      put $ Map.insert ident (CFun (getCType retType) (Prelude.map getArgType args)) env
      return ""

getArgType :: Arg -> CType
getArgType (Arg pos argType ident) = getCType argType

compileDefs :: [TopDef] -> Compl Val
compileDefs [] = do return ""
compileDefs (def : defs) = do
  result <- compileDef def
  results <- compileDefs defs
  return (result ++ "," ++ results)

compileDef :: TopDef -> Compl Val
compileDef (FnDef pos retType ident args block) = do
  env <- get
  loopArgs args
  let (Block pos stmts) = block
  compileStmts stmts
  put env
  return ""

compileStmts :: [Stmt] -> Compl Val
compileStmts [] = do return ""
compileStmts (stmt : stmts) = do
  result <- compileStmt stmt
  results <- compileStmts stmts
  return ""

initVar :: Pos -> CType -> [Item] -> Compl Val
initVar pos varType [] = do return ""
initVar p1 varType ((NoInit p2 ident) : items) = do
  addVar p1 varType ident
initVar p1 varType ((Init p2 ident expr) : items) = do
  addVar p1 varType ident
  compileStmt (Ass p1 ident expr)

compileStmt :: Stmt -> Compl Val
compileStmt (Empty pos) = return ""
compileStmt (BStmt pos block) = do
  let (Block pos stmts) = block
  compileStmts stmts
compileStmt (Decl pos varType items) = do
  initVar pos (getCType varType) items
compileStmt (Ass pos ident expr) = do
  varType <- assertDecl pos ident
  assertExprType expr varType
compileStmt (Incr pos ident) = assertVarType pos ident CInt
compileStmt (Decr pos ident) = assertVarType pos ident CInt
-- compileStmt (Ret pos expr) = return ""
-- compileStmt (VRet) = return ""
-- compileStmts (Cond pos expr stmt)
-- compileStmts (CondElse pos expr stmt1 stmt2)
-- compileStmts (While pos expr stmt)
-- compileStmts (SExp pos expr)
compileStmt _ = do
  -- throwError $ show x
  return ""

assertExprType :: Expr -> CType -> Compl Val
assertExprType (EVar pos ident) exprType = assertVarType pos ident exprType
assertExprType (ELitInt pos _) CInt = return ""
assertExprType (ELitTrue pos) CBool = return ""
assertExprType (ELitFalse pos) CBool = return ""
assertExprType (EOr pos e1 e2) CBool = do
  assertExprType e1 CBool
  assertExprType e2 CBool
assertExprType (EAnd pos e1 e2) CBool = do
  assertExprType e1 CBool
  assertExprType e2 CBool
  return ""
assertExprType (ERel pos e1 op e2) CBool = do
  assertExprType e1 CInt
  assertExprType e2 CInt
assertExprType (EAdd pos e1 op e2) CInt = do
  assertExprType e1 CInt
  assertExprType e2 CInt
assertExprType (EMul pos e1 op e2) CInt = do
  assertExprType e1 CInt
  assertExprType e2 CInt
assertExprType (Not pos expr) CBool = do
  assertExprType expr CBool
assertExprType (Neg pos expr) CInt = do
  assertExprType expr CInt
assertExprType (EString pos string) CStr = return ""
assertExprType (EApp pos ident exprs) expectedType = do
  storedType <- assertDecl pos ident
  case storedType of
    (CFun retType argTypes) ->
      if retType /= expectedType
        then printError pos $ "Function" ++ show ident ++ " should return " ++ show expectedType
        else checkArgTypes pos argTypes exprs
    _ -> printError pos $ show ident ++ " should be a function "
  return ""
assertExprType expr expedtedType = printError (hasPosition expr) $ "Expresion should be of type " ++ show expedtedType

checkArgTypes :: Pos -> [CType] -> [Expr] -> Compl Val
checkArgTypes pos [] [] = return ""
checkArgTypes pos (argType : argTypes) (expr : exprs) = do
  assertExprType expr argType
  checkArgTypes pos argTypes exprs
checkArgTypes pos [] exprs = printError pos $ " expected " ++ show (length exprs) ++ " less arguments"
checkArgTypes pos args [] = printError pos $ " expected " ++ show (length args) ++ " more arguments"

assertVarType :: Pos -> Ident -> CType -> Compl Val
assertVarType pos ident expectedType = do
  env <- get
  case Map.lookup ident env of
    (Just varType) -> do
      if varType == expectedType then return "" else printError pos $ "Variable" ++ show ident ++ " should be of type " ++ show expectedType
      return ""
    Nothing -> printError pos $ show ident ++ " is not declared"

assertDecl :: Pos -> Ident -> Compl CType
assertDecl pos ident = do
  env <- get
  case Map.lookup ident env of
    (Just varType) -> do return varType
    Nothing -> printError pos $ show ident ++ " is not declared"

loopArgs :: [Arg] -> Compl Val
loopArgs [] = do return ""
loopArgs (arg : args) = do
  let (Arg pos argType ident) = arg
  addVar pos (getCType argType) ident
  loopArgs args

addVar :: Pos -> CType -> Ident -> Compl Val
addVar pos varType ident = do
  env <- get
  case Map.lookup ident env of
    (Just _) -> printError pos $ "Name " ++ show ident ++ " is already taken"
    Nothing -> do
      put $ Map.insert ident varType env
      return ""
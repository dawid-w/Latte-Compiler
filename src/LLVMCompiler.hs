module LLVMCompiler where

import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map
import Latte.Abs
import Text.Parsec.Token (GenLanguageDef(identLetter))
import Language.Haskell.TH (varT)
import Types ( Pos, CType(CStr, CBool, CInt, CFun), getCType )
import Prelude

type Env = (Map Ident CType)

type Loc = Int

type Result = String

type Val = String

type Error = String

type Compl a = ExceptT Error (StateT Env IO) a

printError :: Pos -> String -> Compl a
printError (Just (l,c)) text = throwError $ "Line " ++ show l ++" column " ++ show c ++ ": "++ text
printError Nothing text = throwError text

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
    Nothing -> do printError Nothing  "No main function"
    Just main -> do return "Great sucees!"

addDefs :: [TopDef] -> Compl Val
addDefs [] = do return ""
addDefs (def : defs) = do
  result <- addDef def
  results <- addDefs defs
  return (result ++ "," ++ results)

addDef :: TopDef -> Compl Val
addDef (FnDef pos retType ident args block) = do
  env <- get
  --   Check if name is already taken
  case Map.lookup ident env of
    (Just _) -> printError pos $ "Name" ++ show ident ++" is already taken."
    Nothing -> do
      --   TODO: Array of types
      put $ Map.insert ident (CFun (getCType retType) []) env
      return ""

compileDefs :: [TopDef] -> Compl Val
compileDefs [] = do return ""
compileDefs (def : defs) = do
  result <- compileDef def
  results <- compileDefs defs
  return (result ++ "," ++ results)

compileDef :: TopDef -> Compl Val
compileDef (FnDef pos retType ident args block) = do
  env <- get
  -- Add args decl
  loopArgs args
  -- Go through func
  let (Block pos stmts) = block
  compileStmts stmts
  -- block
  --
  put env
  return ""

compileStmts :: [Stmt] -> Compl Val
compileStmts [] = do return ""
compileStmts (stmt : stmts) = do
  result <- compileStmt stmt
  results <- compileStmts stmts
  return ""

compileStmt :: Stmt -> Compl Val
compileStmt (Empty pos) = return ""
-- compileStmt (BStmt block) = return ""
-- compileStmt (Decl varType items) = return ""
compileStmt (Ass pos ident expr) = do
    varType <- assertDecl pos ident
    assertExprType expr varType
compileStmt (Incr pos ident) = assertVarType pos ident CInt
compileStmt (Decr pos ident) = assertVarType pos ident CInt
-- compileStmt (Ret expr) = return ""
-- compileStmt (VRet) = return ""

compileStmt _ = do return ""

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
    -- | EApp Ident [Expr]
assertExprType expr expedtedType = printError  (hasPosition expr) $ "Expresion should be of type " ++ show expedtedType

assertVarType :: Pos -> Ident -> CType -> Compl Val
assertVarType pos ident expectedType = do
  env <- get
  case Map.lookup ident env of
    (Just varType) -> do
      if varType == expectedType then return "" else printError pos $ "Variable" ++ show ident ++ " should be of type " ++ show expectedType
      return ""
    Nothing -> printError pos $ show ident ++ " is not declared"

assertDecl ::Pos-> Ident -> Compl CType
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
    (Just _) -> printError pos $ "Name "++ show  ident ++ " is already taken"
    Nothing -> do
      put $ Map.insert ident varType env
      return ""
module LLVMCompiler where

import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map
import Latte.Abs

type Env = (Map Ident Type)

type Loc = Int

type Result = String

type Val = String

type Error = String

type Compl a = ExceptT Error (StateT Env IO) a

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
compileProgram (Program topDefs) = do
  _ <- addDefs topDefs
  result <- compileDefs topDefs
  env <- get
  case Map.lookup (Ident "main") env of
    Nothing -> do throwError "No main function"
    Just main -> do return "Great sucees!"

addDefs :: [TopDef] -> Compl Val
addDefs [] = do return ""
addDefs (def : defs) = do
  result <- addDef def
  results <- addDefs defs
  return (result ++ "," ++ results)

addDef :: TopDef -> Compl Val
addDef (FnDef retType ident args block) = do
  env <- get
  --   Check if name is already taken
  case Map.lookup ident env of
    (Just _) -> throwError "Name is already taken"
    Nothing -> do
      --   TODO: Array of types
      put $ Map.insert ident (Fun retType []) env
      return ""

compileDefs :: [TopDef] -> Compl Val
compileDefs [] = do return ""
compileDefs (def : defs) = do
  result <- compileDef def
  results <- compileDefs defs
  return (result ++ "," ++ results)

compileDef :: TopDef -> Compl Val
compileDef (FnDef retType ident args block) = do
  env <- get
  -- Add args decl
  loopArgs args
  -- Go through func
  let (Block stmts) = block
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
compileStmt (Incr ident) = do
  _ <- assertOfType ident Int
  env <- get
  return ""
compileStmt _ = do return ""

assertOfType :: Ident -> Type -> Compl Val
assertOfType ident expectedType = do
  env <- get
  case Map.lookup ident env of
    (Just varType) -> do
      unless (varType == expectedType) $ throwError $ "Variable" ++ show ident ++ " should be of type " ++ show expectedType
      return ""
    Nothing -> throwError $ show ident ++ " is not declared"

assertDecl :: Ident -> Compl Val
assertDecl ident = do
  env <- get
  case Map.lookup ident env of
    (Just _) -> do return ""
    Nothing -> throwError $ show ident ++ " is not declared"

loopArgs :: [Arg] -> Compl Val
loopArgs [] = do return ""
loopArgs (arg : args) = do
  let (Arg argType ident) = arg
  addVar argType ident
  return ""

addVar :: Type -> Ident -> Compl Val
addVar varType ident = do
  env <- get
  case Map.lookup ident env of
    (Just _) -> throwError $ "Name " ++ show ident ++ "is already taken"
    Nothing -> do
      put $ Map.insert ident varType env
      return ""
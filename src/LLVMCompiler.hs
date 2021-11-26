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
  result <- addDefs topDefs
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
      let newEnv = Map.insert ident (Fun retType []) env
      put newEnv
      return ""

compileFunc ::
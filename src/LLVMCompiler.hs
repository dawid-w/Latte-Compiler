module LLVMCompiler where

import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map
import Language.Haskell.TH (varT)
import Latte.Abs
import Text.Parsec.Token (GenLanguageDef (identLetter))
import Text.Read.Lex (expect)
import Types (CType (CBool, CFun, CInt, CStr, CVoid), Pos, getCType)
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
    Nothing -> printError Nothing "No main function"
    Just main -> return ""

addDefs :: [TopDef] -> Compl Val
addDefs [] = return ""
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
compileDefs [] = return ""
compileDefs (def : defs) = do
  result <- compileDef def
  results <- compileDefs defs
  return (result ++ "," ++ results)

compileDef :: TopDef -> Compl Val
compileDef (FnDef pos retType ident args block) = do
  env <- get
  loopArgs args
  let (Block pos stmts) = block
  --   validRet <- checkReturn stmts (getCType retType)
  if False
    then do
      printError pos $ "Function needs to return " ++ show (getCType retType)
    else do
      compileStmts (getCType retType) stmts
      put env
      return ""

compileStmts :: CType -> [Stmt] -> Compl Val
compileStmts retType [] = return ""
compileStmts retType (stmt : stmts) = do
  result <- compileStmt retType stmt
  results <- compileStmts retType stmts
  return ""

initVar :: Pos -> CType -> [Item] -> Compl Val
initVar pos varType [] = return ""
initVar p1 varType ((NoInit p2 ident) : items) = do
  addVar p1 varType ident
  initVar p1 varType items
initVar p1 varType ((Init p2 ident expr) : items) = do
  addVar p1 varType ident
  compileStmt CVoid (Ass p1 ident expr)
  initVar p1 varType items

-- checkReturn :: [Stmt] -> CType -> Compl Bool
-- checkReturn [] CVoid = return True
-- checkReturn [] expectedType = return False
-- checkReturn ((Ret pos expr) : stmts) expectedType = do
--   retType <- getExprType expr
--   if retType == expectedType
--     then do
--       res <- checkReturn stmts expectedType
--       return True
--     else printError pos $ "expedted " ++ show expectedType ++ " got " ++ show retType
-- checkReturn ((VRet pos) : stmts) CVoid = do
--   res <- checkReturn stmts CVoid
--   return True
-- checkReturn ((VRet pos) : stmts) expectedType = do
--   printError pos $ "expedted " ++ show expectedType ++ " got void"
-- checkReturn ((Cond pos expr stmt) : stmts) expectedType = do
--   r1 <- checkReturn [stmt] expectedType
--   r2 <- checkReturn stmts expectedType
--   return (r1 || r2)
-- checkReturn ((CondElse pos expr stmt1 stmt2) : stmts) expectedType = do
--   r1 <- checkReturn [stmt1] expectedType
--   r2 <- checkReturn [stmt2] expectedType
--   r3 <- checkReturn stmts expectedType
--   return (r1 || r2 || r3)
-- checkReturn ((While pos expr stmt) : stmts) expectedType = do
--   r1 <- checkReturn [stmt] expectedType
--   r2 <- checkReturn stmts expectedType
--   return (r1 || r2)
-- checkReturn ((BStmt pos block) : stmts) expectedType = do
--   let (Block pos stmts) = block
--   checkReturn stmts expectedType
-- checkReturn (stmt : stmts) expectedType = do
--   checkReturn stmts expectedType

compileStmt :: CType -> Stmt -> Compl Val
compileStmt retType (Empty pos) = return ""
compileStmt retType (BStmt pos block) = do
  let (Block pos stmts) = block
  compileStmts retType stmts
compileStmt retType (Decl pos varType items) =
  initVar pos (getCType varType) items
compileStmt retType (Ass pos ident expr) = do
  varType <- assertDecl pos ident
  assertExprType expr varType
compileStmt retType (Incr pos ident) = assertVarType pos ident CInt
compileStmt retType (Decr pos ident) = assertVarType pos ident CInt
compileStmt retType (Ret pos expr) = do
  r <- getExprType expr
  if retType == r
    then do return ""
    else do printError pos $ "Function should return " ++ show retType
compileStmt CVoid (VRet pos) = return ""
compileStmt retType (VRet pos) = printError pos $ "Function should return " ++ show retType
compileStmt retType (Cond pos expr stmt) = do
  assertExprType expr CBool
  compileStmt retType stmt
compileStmt retType (CondElse pos expr stmt1 stmt2) = do
  assertExprType expr CBool
  compileStmt retType stmt1
  compileStmt retType stmt2
compileStmt retType (While pos expr stmt) = do
  assertExprType expr CBool
  compileStmt retType stmt
compileStmt retType (SExp pos expr) = do
  expType <- getExprType expr
  assertExprType expr expType

getExprType :: Expr -> Compl CType
getExprType (EVar pos ident) = do
  assertDecl pos ident
getExprType (ELitInt pos _) = return CInt
getExprType (ELitTrue pos) = return CBool
getExprType (ELitFalse pos) = return CBool
getExprType (EOr pos e1 e2) = return CBool
getExprType (EAnd pos e1 e2) = return CBool
getExprType (ERel pos e1 op e2) = return CBool
getExprType (EAdd pos e1 op e2) = return CInt
getExprType (EMul pos e1 op e2) = return CInt
getExprType (Not pos expr) = return CBool
getExprType (Neg pos expr) = return CInt
getExprType (EString pos string) = return CStr
getExprType (EApp pos ident exprs) = do
  (CFun retType args) <- assertDecl pos ident
  return retType

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
  retType <- getExprType e1
  assertExprType e1 retType
  assertExprType e2 retType
assertExprType (EAdd pos e1 op e2) CInt = do
  assertExprType e1 CInt
  assertExprType e2 CInt
assertExprType (EAdd pos e1 op e2) CStr = do
  assertExprType e1 CStr
  assertExprType e2 CStr
assertExprType (EMul pos e1 op e2) CInt = do
  assertExprType e1 CInt
  assertExprType e2 CInt
assertExprType (Not pos expr) CBool =
  assertExprType expr CBool
assertExprType (Neg pos expr) CInt =
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
    (Just varType) -> return varType
    Nothing -> printError pos $ show ident ++ " is not declared"

loopArgs :: [Arg] -> Compl Val
loopArgs [] = return ""
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
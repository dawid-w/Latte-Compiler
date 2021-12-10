module StaticAnalysis where

import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map
import Language.Haskell.TH (varT)
import Latte.Abs
import Text.Parsec.Token (GenLanguageDef (identLetter))
import Text.Read.Lex (expect)
import Types (CType (CBool, CFun, CInt, CStr, CVoid), Pos, getCType)
import Prelude

type Env = (Map Ident (CType, Bool))

type Loc = Int

type Result = String

type Val = String

type Error = String

type Compl a = ExceptT Error (StateT Env IO) a

printError :: Pos -> String -> Compl a
printError (Just (l, c)) text = throwError $ "error at line " ++ show l ++ " column " ++ show c ++ ": " ++ text
printError Nothing text = throwError $ "error: " ++ text

initEnv :: Env
initEnv =
  fromList
    [ (Ident "printInt", (CFun CVoid [CInt], False)),
      (Ident "printString", (CFun CVoid [CStr], False)),
      (Ident "error", (CFun CVoid [], False)),
      (Ident "readInt", (CFun CInt [], False)),
      (Ident "readString", (CFun CStr [], False))
    ]

run :: Program -> IO (Either Error String)
run program = do
  co <- runStateT (runExceptT (checkProgram program)) initEnv
  case fst co of
    (Left error) -> return $ Left error
    (Right r) ->
      return $ Right ("Program:" ++ r ++ "\n")

checkProgram :: Program -> Compl Val
checkProgram (Program pos topDefs) = do
  _ <- addDefs topDefs
  result <- checkDefs topDefs
  env <- get
  case Map.lookup (Ident "main") env of
    Nothing -> printError Nothing "No main function"
    Just (CFun CInt [], _) -> return ""
    Just (CFun CInt x, _) -> printError Nothing $ "Main should not have any arguments"
    Just (CFun retType _, _) -> printError Nothing $ "Main needs to return int, not " ++ show retType
    Just (r, _) -> printError Nothing "Main needs to be a function"

addDefs :: [TopDef] -> Compl Val
addDefs [] = return ""
addDefs (def : defs) = do
  result <- addDef def
  results <- addDefs defs
  return ""

addDef :: TopDef -> Compl Val
addDef (FnDef pos retType ident args block) = do
  env <- get
  let (Ident varName) = ident
  case Map.lookup ident env of
    (Just (storedType, modif)) -> printError pos $ "Name" ++ varName ++ " is already taken."
    Nothing -> do
      put $ Map.insert ident (CFun (getCType retType) (Prelude.map getArgType args), False) env
      return ""

getArgType :: Arg -> CType
getArgType (Arg pos argType ident) = getCType argType

checkDefs :: [TopDef] -> Compl Val
checkDefs [] = return ""
checkDefs (def : defs) = do
  result <- checkDef def
  results <- checkDefs defs
  return (result ++ "," ++ results)

checkDef :: TopDef -> Compl Val
checkDef (FnDef pos retType ident args block) = do
  env <- get
  loopArgs args
  let (Block pos stmts) = block
  checkStmts (getCType retType) stmts
  validRet <- checkReturn stmts (getCType retType)
  if not validRet
    then printError pos $ "Function needs to return " ++ show (getCType retType)
    else return ""
  put env
  return ""

checkStmts :: CType -> [Stmt] -> Compl Val
checkStmts retType [] = return ""
checkStmts retType (stmt : stmts) = do
  result <- checkStmt retType stmt
  results <- checkStmts retType stmts
  return ""

initVar :: Pos -> CType -> [Item] -> Compl Val
initVar pos varType [] = return ""
initVar p1 varType ((NoInit p2 ident) : items) = do
  addVar p1 varType ident
  initVar p1 varType items
initVar p1 varType ((Init p2 ident expr) : items) = do
  addVar p1 varType ident
  checkStmt CVoid (Ass p1 ident expr)
  initVar p1 varType items

checkReturn :: [Stmt] -> CType -> Compl Bool
checkReturn [] CVoid = return True
checkReturn [] expectedType = return False
checkReturn ((Ret pos expr) : stmts) expectedType = do
  retType <- getExprType expr
  if retType == expectedType
    then do
      res <- checkReturn stmts expectedType
      return True
    else printError pos $ "expedted " ++ show expectedType ++ " got " ++ show retType
checkReturn ((VRet pos) : stmts) CVoid = do
  res <- checkReturn stmts CVoid
  return True
checkReturn ((VRet pos) : stmts) expectedType =
  printError pos $ "expedted " ++ show expectedType ++ " got void"
checkReturn ((Cond pos (ELitTrue _) stmt) : stmts) expectedType = do
  r1 <- checkReturn [stmt] expectedType
  r2 <- checkReturn stmts expectedType
  return (r1 || r2)
checkReturn ((Cond pos expr stmt) : stmts) expectedType = do
  checkReturn stmts expectedType
checkReturn ((CondElse pos (ELitFalse _) stmt1 stmt2) : stmts) expectedType = do
  r2 <- checkReturn [stmt2] expectedType
  r3 <- checkReturn stmts expectedType
  return (r2 || r3)
checkReturn ((CondElse pos (ELitTrue _) stmt1 stmt2) : stmts) expectedType = do
  r1 <- checkReturn [stmt1] expectedType
  r3 <- checkReturn stmts expectedType
  return (r1 || r3)
checkReturn ((CondElse pos expr stmt1 stmt2) : stmts) expectedType = do
  r1 <- checkReturn [stmt1] expectedType
  r2 <- checkReturn [stmt2] expectedType
  r3 <- checkReturn stmts expectedType
  return ((r1 && r2) || r3)
checkReturn ((While pos (ELitTrue _) stmt) : stmts) expectedType = do
  r1 <- checkReturn [stmt] expectedType
  r2 <- checkReturn stmts expectedType
  return (r1 || r2)
checkReturn ((While pos expr stmt) : stmts) expectedType = do
  checkReturn stmts expectedType
checkReturn ((BStmt pos block) : stmts) expectedType = do
  let (Block pos bStmts) = block
  r1 <- checkReturn bStmts expectedType
  r2 <- checkReturn stmts expectedType
  return (r1 || r2)
checkReturn (stmt : stmts) expectedType =
  checkReturn stmts expectedType

newContext :: (Ident, (CType, Bool)) -> (Ident, (CType, Bool))
newContext (ident, (CFun retType args, modif)) = (ident, (CFun retType args, False))
newContext (ident, (storedType, modif)) = (ident, (storedType, True))

checkStmt :: CType -> Stmt -> Compl Val
checkStmt retType (Empty pos) = return ""
checkStmt retType (BStmt pos block) = do
  let (Block pos stmts) = block
  env <- get
  put $ fromList $ Prelude.map newContext $ toList env
  checkStmts retType stmts
  put env
  return ""
checkStmt retType (Decl pos varType items) =
  initVar pos (getCType varType) items
checkStmt retType (Ass pos ident expr) = do
  varType <- assertDecl pos ident
  assertExprType expr varType
checkStmt retType (Incr pos ident) = assertVarType pos ident CInt
checkStmt retType (Decr pos ident) = assertVarType pos ident CInt
checkStmt retType (Ret pos expr) = do
  r <- getExprType expr
  if retType == r
    then return ""
    else printError pos $ "Function should return " ++ show retType
checkStmt CVoid (VRet pos) = return ""
checkStmt retType (VRet pos) = printError pos $ "Function should return " ++ show retType
checkStmt retType (Cond pos expr stmt) = do
  assertExprType expr CBool
  checkStmt retType stmt
checkStmt retType (CondElse pos expr stmt1 stmt2) = do
  assertExprType expr CBool
  checkStmt retType stmt1
  checkStmt retType stmt2
checkStmt retType (While pos expr stmt) = do
  assertExprType expr CBool
  checkStmt retType stmt
checkStmt retType (SExp pos expr) = do
  expType <- getExprType expr
  assertExprType expr expType

getExprType :: Expr -> Compl CType
getExprType (EVar pos ident) =
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
  let (Ident varName) = ident
  case storedType of
    (CFun retType argTypes) ->
      if retType /= expectedType
        then printError pos $ "Function" ++ varName ++ " should return " ++ show expectedType
        else checkArgTypes pos argTypes exprs
    _ -> printError pos $ varName ++ " should be a function "
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
  let (Ident varName) = ident
  case Map.lookup ident env of
    (Just (varType, modif)) -> do
      if varType == expectedType then return "" else printError pos $ "Variable" ++ varName ++ " should be of type " ++ show expectedType
      return ""
    Nothing -> printError pos $ varName ++ " is not declared"

assertDecl :: Pos -> Ident -> Compl CType
assertDecl pos ident = do
  env <- get
  let (Ident varName) = ident
  case Map.lookup ident env of
    (Just (varType, modif)) -> return varType
    Nothing -> printError pos $ varName ++ " is not declared"

loopArgs :: [Arg] -> Compl Val
loopArgs [] = return ""
loopArgs (arg : args) = do
  let (Arg pos argType ident) = arg
  addVar pos (getCType argType) ident
  loopArgs args

addVar :: Pos -> CType -> Ident -> Compl Val
addVar pos varType ident = do
  env <- get
  let (Ident varName) = ident
  case Map.lookup ident env of
    (Just (storedType, False)) -> printError pos $ "Name " ++ varName ++ " is already defined"
    (Just (storedType, True)) -> do
      put $ Map.insert ident (varType, False) env
      return ""
    Nothing -> do
      put $ Map.insert ident (varType, False) env
      return ""
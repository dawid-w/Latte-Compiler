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

type RegVal = (Int, Result)

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
              ++ "  %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n"
              ++ "  call i32 (i8*, ...) @printf(i8* %t0, i32 %x) \n"
              ++ "  ret void\n"
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
  return $ "define " ++ typeToLL retType ++ " @" ++ name ++ "(" ++ argsStr ++ ") {\n" ++ blockStr ++ "\n}\n"

defArgs :: [Arg] -> Compl Val
defArgs [] = do return ""
defArgs [Arg pos argType ident] = do
  (map, reg) <- get
  put (Map.insert ident (getCType argType, reg) map, reg + 1)
  return $ "i32 %v" ++ show reg
defArgs ((Arg pos argType ident) : args) = do
  argsStr <- defArgs args
  (map, reg) <- get
  put (Map.insert ident (getCType argType, reg) map, reg + 1)
  return $ argsStr ++ ",i32 %v" ++ show reg

compileBlock :: Block -> Compl Val
compileBlock (Block pos stmts) = compileStmts stmts

compileStmts :: [Stmt] -> Compl Val
compileStmts [] = do return ""
compileStmts (stmt : stmts) = do
  r1 <- compileStmt stmt
  r2 <- compileStmts stmts
  return $ r1 ++ r2

compileStmt :: Stmt -> Compl Val
compileStmt (Ret pos expr) = do
  (reg, text) <- compileExpr expr
  return $ text++"\n ret i32 %v" ++ show reg
compileStmt stmt = do return ""

compileExpr :: Expr -> Compl RegVal
compileExpr (EAdd pos e1 op e2) = compileBinExp e1 e2 "add"
compileExpr (ELitInt pos num) = do
  (map, nextR) <- get
  put (map, nextR + 1)
  return (nextR, "%v" ++ show nextR ++ " = " ++ "add" ++ " i32 " ++ "0" ++ ", " ++ show num ++ "\n")
compileExpr (EVar pos ident) = do
  (map, reg) <- get
  put (map, reg + 1)
  let (Ident name) = ident
  case Map.lookup ident map of
    (Just (ctype, varReg)) -> return (reg, "%v" ++ show reg ++ " = add i32 0, %v" ++ show varReg ++ "\n")
    Nothing -> do
      let (Ident name) = ident
      throwError $ "Unknown ident: " ++ name
compileExpr _ = do return (0, "")

compileBinExp :: Expr -> Expr -> String -> Compl RegVal
compileBinExp e1 e2 s = do
  (reg1, result1) <- compileExpr e1
  (reg2, result2) <- compileExpr e2
  (map, reg) <- get
  put (map, reg + 1)
  return (reg, result1 ++ result2 ++ "%v" ++ show reg ++ " = " ++ s ++ " i32 %v" ++ show reg1 ++ ", %v" ++ show reg2 ++ "\n")

module Compiler where

import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map
import Latte.Abs
import Types

type Env = (Map Ident (CType, Bool))

type Loc = Int

type Result = String

type RegNum = Int

type Val = (RegNum, Result)

type Error = String

type Compl a = ExceptT Error (StateT Env IO) a

initEnv :: Env
initEnv =
  fromList
    [ (Ident "printInt", (CFun CVoid [CInt], False)),
      (Ident "printString", (CFun CVoid [CStr], False)),
      (Ident "error", (CFun CVoid [], False)),
      (Ident "readInt", (CFun CInt [], False)),
      (Ident "readString", (CFun CStr [], False))
    ]

compile :: Program -> IO (Either Error String)
compile program = do
  -- return $ Right "Result"
  result <- runStateT (runExceptT (compileProgram program)) initEnv
  case fst result of
    (Left error) -> return $ Left error
    (Right (_, resultText)) ->
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
compileDefs [] = return (0, "")
compileDefs (def : defs) = do
  (_, resultText1) <- compileDef def
  (_, resultText2) <- compileDefs defs
  return (0, resultText1 ++ resultText2)

compileDef :: TopDef -> Compl Val
compileDef (FnDef pos retType (Ident name) args block) = do
  return
    ( 0,
      "define " ++ typeToLL retType ++ " @" ++ name ++ "(i32 %n) {\n"
        ++ "ret i32 0\n"
        ++ "}\n"
    )
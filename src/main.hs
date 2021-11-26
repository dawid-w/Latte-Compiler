module Main where

import LLVMCompiler
import Latte.Abs
import Latte.Par
import System.Environment (getArgs)
import System.FilePath (dropExtension, replaceExtension, takeDirectory, takeFileName)
import System.Process
import Text.Parsec.Prim (putState)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putStr "TODO\n"
    [filename] -> do
      code <- readFile filename
      let tokens = myLexer code

      case pProgram tokens of
        Right program -> do
          result <- compile program
          case result of
            (Right text) -> do
              putStrLn $ "OK: " ++ text
            (Left error) -> do
              putStrLn error
        Left error -> do
          putStr ("Error while parsing:\n" ++ error ++ "\n")
      return ()
    _ -> putStr "<Help>\n"
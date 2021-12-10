module Main where

import Compiler
import Latte.Abs
import Latte.Par
import StaticAnalysis
import System.Environment (getArgs)
import System.Exit
import System.FilePath (dropExtension, replaceExtension, takeDirectory, takeFileName)
import System.IO (hPrint, hPutStr, hPutStrLn, stderr)
import System.Process
import Text.Parsec.Prim (putState)

helpStr :: String
helpStr =
  "---- Latte compiler ----\n\n"
    ++ "Compile latte file:\n"
    ++ "./latc <filename>\n\n"
    ++ "------------------------\n"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putStr helpStr
    [filename] -> do
      code <- readFile filename
      let tokens = myLexer code

      case pProgram tokens of
        Right program -> do
          result <- run program
          case result of
            (Right text) -> do
              putStrLn "OK\n"
              compilerResult <- compile program
              -- TODO: --
              case compilerResult of
                (Right generatedText) -> putStrLn generatedText
                (Left error) -> hPutStrLn stderr $ "ERROR\n" ++ error ++ "\n"
              exitSuccess
            --------------
            (Left error) -> do
              hPutStrLn stderr $ "ERROR\n" ++ error ++ "\n"
              exitFailure
        Left error -> do
          hPutStrLn stderr $ "ERROR\n" ++ error ++ "\n"
          exitFailure
      return ()
    _ -> putStr helpStr

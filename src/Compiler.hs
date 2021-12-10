module Compiler where

import Latte.Abs

type Error = String

compile :: Program -> IO (Either Error String)
compile program = do
  return $ Right "Result"
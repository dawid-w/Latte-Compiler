module Env where

import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map
import Latte.Abs
import Types

type Error = String

type RegNum = Int

type Compl a = ExceptT Error (StateT Env IO) a

type Env = (Map Ident (CType, RegNum), RegNum)

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

addVar :: CType -> Ident -> Compl RegNum
addVar varType ident = do
  (map, newReg) <- get
  let (Ident varName) = ident
  put (Map.insert ident (varType, newReg) map, newReg + 1)
  return newReg
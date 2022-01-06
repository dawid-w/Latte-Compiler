module Env where

import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map
import Latte.Abs
import StaticAnalysis (printError)
import Types

type Error = String

type Loc = Int

type VEnv = Map Ident Loc

type PEnv = Map Ident CType

type Store = Map Loc (CType, Var)

type Env = (PEnv, VEnv, Store, Loc, Register, Label, Var)

type Compl a = ExceptT Error (StateT Env IO) a

initEnv :: Env
initEnv =
  ( fromList
      [ (Ident "printInt", CFun CVoid [CInt]),
        (Ident "printString", CFun CVoid [CStr]),
        (Ident "error", CFun CVoid []),
        (Ident "readInt", CFun CInt []),
        (Ident "concat", CFun CStr [CStr, CStr]),
        (Ident "readString", CFun CStr [])
      ],
    Map.empty,
    Map.empty,
    0,
    Reg 0,
    Lab 0,
    Var 0
  )

addVar :: CType -> Ident -> Compl Var
addVar varType ident = do
  (penv, venv, store, loc, reg, label, var) <- get
  put (penv, Map.insert ident loc venv, Map.insert loc (varType, var) store, loc + 1, reg, label, nextVar var)
  return var

addProc :: CType -> Ident -> [CType] -> Compl ()
addProc retType ident argsTypes = do
  (penv, venv, store, loc, reg, label, var) <- get
  put (Map.insert ident (CFun retType argsTypes) penv, venv, store, loc, reg, label, var)
  return ()

useReg :: Compl Register
useReg = do
  (penv, venv, store, loc, reg, label, var) <- get
  put (penv, venv, store, loc, nextReg reg, label, var)
  return reg

useLabel :: Compl Label
useLabel = do
  (penv, venv, store, loc, reg, label, var) <- get
  put (penv, venv, store, loc, reg, nextLabel label, var)
  return label

getVar :: Ident -> Compl (CType, Var)
getVar ident = do
  (penv, venv, store, loc, reg, label, var) <- get
  let (Just varLoc) = Map.lookup ident venv
  let (Just (ctype, var)) = Map.lookup varLoc store
  return (ctype, var)

getProc :: Ident -> Compl (CType, [CType])
getProc ident = do
  (penv, venv, store, loc, reg, label, var) <- get
  let (Just (CFun retType argsTypes)) = Map.lookup ident penv
  return (retType, argsTypes)

lastVar :: Compl Var
lastVar = do
  (penv, venv, store, loc, reg, label, Var num) <- get
  return (Var $ num -1)

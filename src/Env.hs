module Env where

import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map
import Latte.Abs
import StaticAnalysis (printError)
import Types

type Error = String

-- type RegNum = Int

type Loc = Int

-- Ident -> Loc
type VEnv = Map Ident Loc

type PEnv = Map Ident CType

-- Loc -> Typ zmiennej, w jakim jest obecnie rejestrze
type Store = Map Loc (CType, Register)

type Env = (PEnv, VEnv, Store, Loc, Register, Label)

type Compl a = ExceptT Error (StateT Env IO) a

initEnv :: Env
initEnv =
  ( fromList
      [ (Ident "printInt", CFun CVoid [CInt]),
        (Ident "printString", CFun CVoid [CStr]),
        (Ident "error", CFun CVoid []),
        (Ident "readInt", CFun CInt []),
        (Ident "readString", CFun CStr [])
      ],
    Map.empty,
    Map.empty,
    0,
    Reg 0,
    Lab 0
  )

addVar :: CType -> Ident -> Compl Register
addVar varType ident = do
  (penv, venv, store, loc, reg, label) <- get
  put (penv, Map.insert ident loc venv, Map.insert loc (varType, reg) store, loc + 1, nextReg reg, label)
  return reg

addProc :: CType -> Ident -> [CType] -> Compl ()
addProc retType ident argsTypes = do
  (penv, venv, store, loc, reg, label) <- get
  put (Map.insert ident (CFun retType argsTypes) penv, venv, store, loc, reg, label)
  return ()

setVar :: CType -> Ident -> Compl Register
setVar varType ident = do
  (penv, venv, store, loc, reg, label) <- get
  let (Just varLoc) = Map.lookup ident venv
  put (penv, venv, Map.insert varLoc (varType, reg) store, loc, nextReg reg, label)
  return reg

useReg :: Compl Register
useReg = do
  (penv, venv, store, loc, reg, label) <- get
  put (penv, venv, store, loc, nextReg reg, label)
  return reg

useLabel :: Compl Label
useLabel = do
  (penv, venv, store, loc, reg, label) <- get
  put (penv, venv, store, loc, reg, nextLabel label)
  return label

getVar :: Ident -> Compl (CType, Register)
getVar ident = do
  (penv, venv, store, loc, reg, label) <- get
  let (Just varLoc) = Map.lookup ident venv
  let (Just (ctype, varReg)) = Map.lookup varLoc store
  return (ctype, varReg)

getProc :: Ident -> Compl (CType, [CType])
getProc ident = do
  (penv, venv, store, loc, reg, label) <- get
  let (Just (CFun retType argsTypes)) = Map.lookup ident penv
  return (retType, argsTypes)
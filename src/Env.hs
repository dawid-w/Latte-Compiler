module Env where

import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map
import Latte.Abs
import Types

type Error = String

-- type RegNum = Int

type Compl a = ExceptT Error (StateT Env IO) a

type Loc = Int

-- Ident -> Loc
type VEnv = Map Ident Loc

-- Loc -> Typ zmiennej, w jakim jest obecnie rejestrze
type Store = Map Loc (CType, Register)

type Env = (VEnv, Store, Loc, Register)

initEnv :: Env
initEnv = (Map.empty, Map.empty, 0, Reg 0)

--   ( fromList
--       [ (Ident "printInt", (CFun CVoid [CInt], Reg 0)),
--         (Ident "printString", (CFun CVoid [CStr], Reg 0)),
--         (Ident "error", (CFun CVoid [], Reg 0)),
--         (Ident "readInt", (CFun CInt [], Reg 0)),
--         (Ident "readString", (CFun CStr [], Reg 0))
--       ],
--     Reg 0
--   )

addVar :: CType -> Ident -> Compl Register
addVar varType ident = do
  (venv, store, loc, reg) <- get
  put (Map.insert ident loc venv, Map.insert loc (varType, reg) store, loc + 1, nextReg reg)
  return reg

setVar :: CType -> Ident -> Compl Register
setVar varType ident = do
  (venv, store, loc, reg) <- get
  let (Just varLoc) = Map.lookup ident venv
  --   (ctype, reg) <- getVar ident
  put (venv, Map.insert varLoc (varType, reg) store, loc, nextReg reg)
  return reg

useReg :: Compl Register
useReg = do
  (venv, store, loc, reg) <- get
  put (venv, store, loc, nextReg reg)
  return reg

getVar :: Ident -> Compl (CType, Register)
getVar ident = do
  (venv, store, loc, reg) <- get
  let (Just varLoc) = Map.lookup ident venv
  let (Just (ctype, varReg)) = Map.lookup varLoc store
  return (ctype, varReg)

--   (map, nextR) <- get
--   case Map.lookup ident map of
--     (Just (ctype, varReg)) -> do
--       nextR <- addVar ctype ident
--       return $ text ++ show nextR ++ " = or " ++ show exprType ++ " 0, " ++ show reg ++ "\n"
--     Nothing -> do
--       let (Ident name) = ident
--       throwError $ "Unkfnown ident: " ++ name

-- setVar :: Ident -> Compl Register
-- setVar varType ident = do
--   (venv, store, loc, reg) <- get
--   put (Map.insert ident loc venv, Map.insert loc (varType, reg) store, loc + 1, nextReg reg)
--   return reg

--   let (Ident varName) = ident

--   (map, Reg newRegNum) <- get
--   let (Ident varName) = ident
--   put (Map.insert ident (varType, Reg newRegNum) map, Reg $ newRegNum + 1)
--   return $ Reg newRegNum
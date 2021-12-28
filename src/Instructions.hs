module Instructions where

import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map
import Latte.Abs
import Types

data Instruction
  = ArtI ArtOp Val Val Register
  deriving (Eq)

instance Show Instruction where
  show (ArtI op v1 v2 reg) = show reg ++ " = " ++ show op ++ " i32 " ++ show v1 ++ ", " ++ show v2 ++ "\n"

data ArtOp
  = AddOp
  | SubOp
  deriving (Eq)

instance Show ArtOp where
  show AddOp = "add"
  show SubOp = "sub"
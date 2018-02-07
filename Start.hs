module Start where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

type Name = Char

data Expr = 
   Var Name
 | And Expr Expr
 | Or Expr Expr
 | Not Expr
 | Const Bool
 deriving (Show, Eq)

-- free variables of an Expr
fvs :: Expr -> Set Name
fvs (Const _) = Set.empty
fvs (Var v) = Set.singleton v
fvs (Not e) = fvs e 
fvs (Or x y) = Set.union (fvs x) (fvs y)
fvs (And x y) = Set.union (fvs x) (fvs y)

type BoolMap = Map Name Bool

-- satisfiable [[x ∨ y]] = 
--   [ [x↦T,y↦T]
--   , [x↦T,y↦F]
--   , [x↦F,y↦T]
--   ]
satisfiable :: Expr -> BoolMap -> [BoolMap]
satisfiable e env = undefined

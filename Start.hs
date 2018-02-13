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

--Got this off of StackOverflow. Generates superset of list.
--Ex: ['a', 'b'] -> ['', 'a', 'b', 'ab']
subseq :: [a] -> [[a]]
subseq [] = [[]]
subseq (x:xs) = (subseq xs) ++ map (x :) (subseq xs)

--Given two Strings, the first being the list of free variables,
--and the second being the current domain, generate a BoolMap.
--Ex: if our free variables are 'a', 'b', and 'c', and our domain is
--'a', 'c', the generated BoolMap will be: 
--{'a': True, 'b': False, 'c': True}
allP :: String -> String -> BoolMap
allP [] _ = Map.empty
allP [x] domain = if x `elem` domain
           then Map.insert x True Map.empty
           else Map.insert x False Map.empty
allP (x:xs) domain
  | x `elem` domain = Map.union (Map.insert x True Map.empty) (allTail)
  | otherwise      = Map.union (Map.insert x False Map.empty) (allTail)
  where allTail = allP xs domain

--Given an expression, generates all possible BoolMaps.
allPossibleMaps :: Expr -> [BoolMap]
allPossibleMaps expr =
  [allP (Set.toList (fvs expr)) x| x <- vars]
  where vars = subseq (Set.toList (fvs expr)) 

satisfiable :: Expr -> BoolMap -> [BoolMap]
satisfiable e env = [Map.fromList [('c', True)] ]

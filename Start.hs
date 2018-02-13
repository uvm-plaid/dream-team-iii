module Start where

import Data.Maybe

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

--Given a BoolMap and an expression, replaces all free variables
--in the expression with their values in the BoolMap
--Ex: 
--  e    = (And (And (Var 'a') (Var 'b')) (Var 'c'))
--  bmap = {a : True, b : False, c : True}
--  ->     (And (And (Const True) (Const False)) (Const True)) 
injectMapValues :: BoolMap -> Expr -> Expr
injectMapValues bmap e =
  case e of
    Var v -> Const (Data.Maybe.fromJust (Map.lookup v bmap))
    Not e -> Not (inject e)
    Or x y -> Or (inject x) (inject y)
    And x y -> And (inject x) (inject y)
    Const b -> Const b
  where
    inject = injectMapValues bmap   

-- Stolen from the blog
-- Extract the boolean from the Const constructor.
unConst :: Expr -> Bool
unConst (Const b) = b
unConst _ = error "Not Const"

-- Also stolen from the blog
simplify :: Expr -> Expr
simplify (Const b) = Const b
simplify (Var v) = Var v
simplify (Not e) =
  case simplify e of
    Const b -> Const (not b)
    e -> Not e
simplify (Or x y) =
  -- Get rid of False values, which are irrelevant.
  let es = filter (/= Const False) [simplify x, simplify y]
  in
    -- If True is in a branch, the entire expression is True.
    if Const True `elem` es
    then Const True
    else
      case es of
        -- If all the values were False, this 'or' is unsatisfied.
        [] -> Const False
        [e] -> e
        [e1, e2] -> Or e1 e2
-- Dual to the simplify (Or x y) definition.
simplify (And x y) =
  let es = filter (/= Const True) [simplify x, simplify y]
  in
    if Const False `elem` es
    then Const False
    else
      case es of
        [] -> Const True
        [e] -> e
        [e1, e2] -> And e1 e2

--Given a BoolMap and Expression, returns whether or not
--BoolMap can be used to satisfy expression
canSatisfy :: BoolMap -> Expr -> Bool
canSatisfy b e = unConst (simplify (injectMapValues b e))

--Generates all possible BoolMaps that can be used to satisfy
--a given expression. Return an empty list if none exist.
satisfiable :: Expr -> [BoolMap]
satisfiable e = [bm | bm <- allPossibleMaps e, canSatisfy bm e]

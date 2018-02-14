module Main where


--plan
--get all free variables of expressions
--generate power set of it
--map all combinations of true and false
--eliminate combinations that dont satisfy expressions


--new plan
--no need for powerset, if variable left unmapped, cant satisfy expression
--map all combos of two lists [free variables] and [true, false]
--substitute each "guess" of mappings into expressions
--if satisfies expression, save it. if not, throw it out
--return list of lists that  satisfies expression


import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

type Name = Char

main :: IO ()
main = do
  putStrLn "Hello"

data Expr = 
    Var Name
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Const Bool
  deriving (Show, Eq)

--Return the set of free variables
freeVariables :: Expr -> Set Name
freeVariables (Const _) = Set.empty
freeVariables (Var v) = Set.singleton v
freeVariables (Not e) = freeVariables e
freeVariables (Or x y) = Set.union (freeVariables x) (freeVariables y)
freeVariables (And x y) = Set.union (freeVariables x) (freeVariables y)

type BoolMap = Map Name Bool

-- satisfiable [[x ∨ y]] =
--   [ [x↦T,y↦T]
--   , [x↦T,y↦F]
--   , [x↦F,y↦T]
--   ]


unConst :: Expr -> Bool
unConst (Const b) = b
unConst _ = error "Not Const"

satisfiable :: Expr -> BoolMap -> [BoolMap]
satisfiable e env = 
  case freeVariables expr of
    Set.empty -> unConst expr
    otherwise -> let v = Set.toList (freeVariables expr)!!1
                 in
                   if satisfiable simplify (guessVariable v True expr)
                   then Set.inset (BoolMap v True) env
                   else 
                     if satisfialble simplify (guessVariable v False expr)
                     then Set.insert (BoolMap v False) env
                     else []


--data type mismatch?
--allBoolMaps :: [Name] -> [[BoolMap]]
--allBoolMaps [] = [[]]
--allBoolMaps x:xs = [ [BoolMap x True, allBoolMaps xs] , [BoolMap x False, allBoolMaps xs] ]

{-
--gives all permutations of BoolMap ----still brute force?
allBoolMaps :: Expr -> [BoolMap]
allBoolMaps expr = internalAllBoolMaps set | set <- powerset (Set.toList (freeVariables expr))

internalAllBoolMaps :: [Name] -> [BoolMap]
internalAllBoolMaps [] = []
internalAllBoolMaps x:xs = [[BoolMap x True], [BoolMap x False]] ++ 


--gives power set of free varialbes
--source: https://mail.haskell.org/pipermail/haskell-cafe/2003-June/004484.html
powerset :: [a] -> [[a]]
powerset []   = [[]]
powerset x:xs = xss /\/ map (x:) xss
                where xss = powerset xs
-}




sVariable :: Char -> Bool -> Expr -> Expr
guessVariable var val e =
  case e of 
    Var v -> if v == var
             then Const val
             else Var v
    Not e -> Not (guess e)
    Or x y -> Or (guess x) (guess y)
    And x y -> And (guess x) (guess y)
    Const b -> Const b
  where
    guess = guessVariable var val

simplify :: Expr -> Expr
simplify (Const b) = Const b
simplify (Var v) = Var v
simplify (Not e) =
  case simplify e of
    Const b -> Const (not b)
    e -> Not e
simplify (Or x y) =
  let es = filter (/= Const False) [simplify x, simplify y]
  in
    if Const True `elem` es
    then Const True
    else
      case es of
        [] -> Const False
        [e] -> e
        [e1, e2] -> Or e1 e2
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

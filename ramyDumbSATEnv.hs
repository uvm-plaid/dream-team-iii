{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SATEnv where

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad

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

-- ‣ This is an evaluation function which, given ⸨env⸩ as a mapping from free
--   variables to boolean values, fully evaluates the SAT term to a boolean
--   value, so either True or False.
-- ‣ Notice that we are not performing substitution one variable at a time,
--   rather these substitutions are "delayed" until the evaluator needs to know
--   the value of a variable.
-- ⋆ Invariant: assume that for any call to ⸨eval e env⸩, if ⸨x ∈ fvs e⸩ then
--   ⸨env[x]⸩ is defined.
eval :: Expr -> BoolMap -> Bool
eval (Const b) env = b
eval (Var x) env = env Map.! x
eval (Not e) env = not (eval e env)
eval (Or x y) env = eval x env || eval y env
eval (And x y) env = eval x env && eval y env

-- ‣ Builds up all possible environments through recursive calls to
--   ⸨satisfiableVars⸩
-- ⋆ Invariant: assume that xs is some subset of the free variables in e
satisfiableVars :: [Name] -> Expr -> BoolMap -> [BoolMap]
satisfiableVars xs e env = case xs of
  [] -> 
    if eval e env == True 
      then [env]
      else []
  x:xs' -> 
    satisfiableVars xs' e (Map.insert x True env)
    ++
    satisfiableVars xs' e (Map.insert x False env)

-- ‣ Runs the expression in all possible environments, by starting the
-- ⸨satisfiableVars⸩ with the complete list of free variables in ⸨e⸩, and the
-- empty environment mapping.
satisfiable :: Expr -> [BoolMap]
satisfiable e =
  let xs = Set.toList (fvs e)
  in satisfiableVars xs e Map.empty

-- Run this with:
--     > runhaskell SATEnv.hs
main :: IO ()
main = do
  let e :: Expr
      e = And (Or (Var 'a') (Var 'b')) (Var 'c')
  putStrLn (show (satisfiable e))

-- Homework: rewrite eval and satisfiableVars to use the reader monad The
-- comment [!!] is where you should start writing code, but make sure you
-- understand the code in-between first.

-- we could have also defined `Reader` and `unReader` in one line as follows:
--
--     newtype Reader r a = Reader { unReader :: r -> a }
--
data Reader r a = Reader (r -> a)

unReader :: forall r a. Reader r a -> (r -> a)
unReader (Reader f) = f

-- we could define this but it isn't very useful, because we can always just
-- write `Reader f`
--
--     mkReader :: forall r a. (r -> a) -> Reader r a
--     mkReader f = Reader f

return_unwrapped :: forall r a. a -> (r -> a)
return_unwrapped x = \ _ -> x

instance Monad (Reader r) where
  return :: forall a. a -> Reader r a
  return x = Reader $ \ _ -> x

  (>>=) :: forall a b. Reader r a -> (a -> Reader r b) -> Reader r b
  xM >>= k = Reader $ \ r ->
    let x = unReader xM r
    in unReader (k x) r
instance Functor (Reader r) where {fmap = liftM}
instance Applicative (Reader r) where {pure = return;(<*>) = ap}

--          the enviornment    the "result" of the computation
--  the monad    |             | 
--        ⌄      ⌄      ⌄-------
ask :: Reader BoolMap BoolMap
ask = Reader $ \ r -> r

local :: forall a. BoolMap -> Reader BoolMap a -> Reader BoolMap a
local env xM = Reader $ \ _ -> unReader xM env

-- example of looking up a variable 'x' and 'y' from the reader environment
--              the enviornment    the "result" of the computation
--      the monad    |             | 
--            ⌄      ⌄      ⌄-------
lookup_x_annotated :: Reader BoolMap Bool
lookup_x_annotated = do
  (env :: BoolMap) <- (ask :: Reader BoolMap BoolMap)
  let result :: Bool = env Map.! 'x'
  (return result) :: Reader BoolMap Bool

lookup_x :: Reader BoolMap Bool
lookup_x = do
  env <- ask
  let result = env Map.! 'x'
  return result

lookup_x_guts :: BoolMap -> Bool
lookup_x_guts = unReader lookup_x

lookup_y :: Reader BoolMap Bool
lookup_y = do
  env <- ask
  return (env Map.! 'y')

-- example of extending the current environment

flip_x :: forall a. Reader BoolMap a -> Reader BoolMap a
flip_x xM = do
  env <- ask
  let b = env Map.! 'x'
  local (Map.insert 'x' (not b) env) xM

-- example of combining lookup and increment

example :: Reader BoolMap (Bool,Bool,Bool,Bool)
example = do
  x1 <- lookup_x
  y1 <- lookup_y
  flip_x $ do
    x2 <- lookup_x
    y2 <- lookup_y
    return (x1,y1,x2,y2)

-- run this function by loading this file in ghci:
--     > ghci SATEnv.hs
-- and then evaluating exampleMain
--     > exampleMain
--     (True,False,False,False)
-- alternatively, comment out the definition of main above, rename this to
-- main, and run with runhaskell:
--     > runhaskell SATEnv.hs
exampleMain :: IO ()
exampleMain = do
  putStrLn (show (unReader example (Map.fromList [('x',True),('y',False)])))

-- [!!] Complete these definitions...

evalM :: Expr -> Reader BoolMap Bool
--unReader Reader BoolMap Bool is the same as BoolMap -> Bool
evalM (Const b) = return b
evalM (Var x) = do
  env <- ask 
  return (env Map.! x) 
evalM (Not e)  = do
  env <- ask
  return (not ((unReader (evalM e)) env))
evalM (Or x y)  = do
  env <- ask
  return (((unReader (evalM x)) env) || ((unReader (evalM y))) env ) 
evalM (And x y)  = do 
  env <- ask
  return (((unReader (evalM x)) env) && ((unReader (evalM y))) env ) 

satisfiableVarsM :: [Name] -> Expr -> Reader BoolMap [BoolMap]
satisfiableVarsM xs e = do
  env <- ask
  case xs of
    [] -> 
      if ((unReader (evalM e)) env) == True
        then return [env]
        else return []
    x:xs' -> 
      return (
      ((unReader (satisfiableVarsM xs' e)) (Map.insert x True env))
      ++
      ((unReader (satisfiableVarsM xs' e)) (Map.insert x False env))
      )

satisfiableM :: Expr -> [BoolMap]
satisfiableM e =
  let xs = Set.toList (fvs e) 
  in do
    (unReader (satisfiableVarsM xs e)) Map.empty

-- run this function by loading this file in ghci:
--     > ghci SATEnv.hs
-- and then evaluating mainM
--     > mainM
--     <result>
-- alternatively, comment out the definition of main above, rename this to
-- main, and run with runhaskell:
--     > runhaskell SATEnv.hs
mainM :: IO ()
mainM = do
  let e :: Expr
      e = And (Or (Var 'a') (Var 'b')) (Var 'c')
  putStrLn (show (satisfiableM e))

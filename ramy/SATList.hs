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
eval :: Expr -> (BoolMap -> Bool)
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

-- [!!] Assignment

newtype ListReader r a = ListReader { unListReader :: r -> [a] }

instance Monad (ListReader r) where
  return :: forall a. a -> ListReader r a
  return x = ListReader $ \ _ -> [x]

  (>>=) :: forall a b. ListReader r a -> (a -> ListReader r b) -> ListReader r b
  xM >>= k = ListReader $ \ env ->
    let xs = unListReader xM env
    in foldr (\ x ys -> unListReader (k x) env ++ ys) [] xs
instance Functor (ListReader r) where {fmap = liftM}
instance Applicative (ListReader r) where {pure = return;(<*>) = ap}

ask :: forall r. ListReader r r
ask = ListReader $ \ r -> [r]

local :: forall r a. r -> ListReader r a -> ListReader r a
local env xM = ListReader $ \ _ -> unListReader xM env

none :: forall r a. ListReader r a
none = ListReader $ \ _ -> []

(<+>) :: forall r a. ListReader r a -> ListReader r a -> ListReader r a
xM <+> yM = ListReader $ \ env -> unListReader xM env ++ unListReader yM env

-- some warmup examples...
--
-- load this file in ghci, evaluate each example, and make sure you understand
-- each the result

runExample :: ListReader Int a -> [a]
runExample xM = unListReader xM 999

e1 :: [Int]
e1 = runExample $ do
  return 1

e2 :: [Int]
e2 = runExample $ do
  ask

e3 :: [Int]
e3 = runExample $ do
  return 1 <+> return 2

e4 :: [Int]
e4 = runExample $ do
  return 1 <+> ask

e5 :: [Int]
e5 = runExample $ do
  return 1 <+> none

e6 :: [Int]
e6 = runExample $ do
  x <- (return 1 <+> return 2)
  return (x + 1)

e7 :: [Int]
e7 = runExample $ do
  x <- (return 1 <+> return 2)
  y <- (return 10 <+> return 20)
  return (x + y)

e8 :: [Int]
e8 = runExample $ do
  x <- none
  return 1

e9 :: [Int]
e9 = runExample $ do
  x <- (ask <+> local 111 ask)
  y <- (return x <+> none)
  return (x + y)

-- Your assignment...
--
-- We won't use M, but this is just to reinforce that the monad at play here is:
-- M a = (ListReader BoolMap) a
type M a = ListReader BoolMap a

-- Fill in the undefined parts. Hints are given as to what operations you
-- should use. In general, you should only use:
-- - do notation (do { x <- e ; e })
-- - return
-- - ask
-- - local
-- - <+>
-- - none
--
-- Avoid "opening up" the abstraction, that is, don't use the constructor
-- ListReader or the destructor unListReader
--
-- If you find a solution that works that doesn't use the hints, that's okay.
-- There may be more than one way to implement things. If you get stuck, ping
-- me on slack.

-- Hints: use do notation, ask and return
evalM :: Expr -> ListReader BoolMap Bool
evalM (Const b) = return b
evalM (Var x) = do
  env <- ask
  return (env Map.! x)
evalM (Not e) = do
  b <- evalM e
  return (not b)
evalM (Or x y) = do
  b1 <- evalM x
  b2 <- evalM y
  return (b1 || b2)
evalM (And x y) = do
  b1 <- evalM x
  b2 <- evalM y
  return (b1 && b2)

-- This is slightly modified from before. Previously `satisfiableVarsM` was
-- recursive, and called `local` around recursive calls to extend the
-- environment. We will instead use a helper function `allEnvs` which returns
-- all possible environment assigments of variables to booleans. Remember,
-- `allEnvs` shouldn't just return one BoolMap, it should return many. However,
-- because we are inside a list monad, you should use the list monad operation
-- `<+>` when you need to combine multiple monadic results.
--
-- Hints: use do notation, ask, local and <+>

allEnvs :: [Name] -> ListReader BoolMap BoolMap
allEnvs [] = ask
allEnvs (x:xs) = do
  env <- ask
  let first = local (Map.insert x True env) (allEnvs xs)
      second = local (Map.insert x False env) (allEnvs xs)
  first <+> second
  
  

-- The "magic" here is the first line. `allEnvs (Set.toList (fvs e))` will
-- "return" its result, bound to `env`, *multiple times*. This means that the
-- code that follows that line will be run multiple times, for each environment
-- returned by `allEnvs`.
--
-- Hints: use do notation, return and none
satisfiableVarsM :: Expr -> ListReader BoolMap BoolMap
satisfiableVarsM e = do
  env <- allEnvs (Set.toList (fvs e))
  b <- local env (evalM e)
  if b == True
    then (none <+> return env)   --related to e9?
        --all typecheck but get a Map.! exception
        --return env
        -- (none <+> return env)
        -- (ask <+> return env)
    else none

satisfiableM :: Expr -> [BoolMap]
satisfiableM e = unListReader (satisfiableVarsM e) Map.empty

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


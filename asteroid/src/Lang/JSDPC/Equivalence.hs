module Lang.JSDP.Equivalence where

import Lib

import Lang.JSDP.Syntax

-- e.g., if(x){if(y){a}{b}}{if(z){d}{e}}
--          ^
--      no nesting
--
-- in constructor form
-- Link "x" (Link "y" (Leaf "a") (Leaf "b")) (Link "z" (Leaf "d") (Leaf "e"))
--
-- INVARIANT
-- The variables mentioned in the link chain should be in strictly ascending
-- order, that is:
-- 
--     if(x){if(y){a}{b}}{if(z){d}{e}}
--
-- is a valid IfChain because x < y and x < z. The following IfChain is
-- therefore invalid:
--
--     if(y){if(x){a}{b}}{if(z){d}{e}}
--
-- because y ≮ z
--
-- also, this chain is not valid either:
--
--     if(x){if(y){a}{b}}{if(x){d}{e}}
--
-- because x ≮ x

data IfChain = 
    Leaf 𝕊
  | Link 𝕊 IfChain IfChain

-- !! complete unnormalize
-- NF is a "sum of products of if-chains" representation.
type NF = 𝑃 (𝐿 IfChain)
unnormalize ∷ NF → Exp
unnormalize sps = undefined
  -- foldr𝐿 (Lit False) Join
  -- $ map𝐿 (foldr𝐿 (Lit True) DProd)
  -- $ map𝐿 (map𝐿 Var) 
  -- $ list𝑃 sps

-- !! complete normalize
normalize 
  ∷ Exp  -- ^ The JSDP expression
  → NF   -- ^ The normalized expression.
normalize e = case e of
  Lit b -> undefined
    -- case b of
    -- True ->
    --     set [list []]
    -- False ->
    --     set []
  Var x -> undefined
    -- set [list [x]]
  Join x y -> undefined
    -- (∪) (normalize x) (normalize y)
  DProd x y -> undefined
    -- set𝐿 $ cartWith (⧺) (list𝑃 (normalize x)) (list𝑃 (normalize y))
  
equiv ∷ Exp → Exp → 𝔹
equiv e₁ e₂ = normalize e₁ ≟ normalize e₂

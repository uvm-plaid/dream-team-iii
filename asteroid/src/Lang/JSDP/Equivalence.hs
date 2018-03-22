module Lang.JSDP.Equivalence where

import Lib

import Lang.JSDP.Syntax

-- NF is a "sum of products" representation.
type NF = 𝑃 (𝐿 𝕊)

-- A mapping back from normal forms to expressions:
unnormalize ∷ NF → Exp
unnormalize sps = 
  foldr𝐿 (Lit False) Join
  $ map𝐿 (foldr𝐿 (Lit True) DProd)
  $ map𝐿 (map𝐿 Var) 
  $ list𝑃 sps

-- [!!] TODO
-- it should be the case that `unnormalize (normalize e)` returns an
-- "equivalent" formula, modulo the laws shown in Syntax.hs.
normalize 
  ∷ Exp  -- ^ The JSDP expression
  → NF   -- ^ The normalized expression.
normalize e = undefined

equiv ∷ Exp → Exp → 𝔹
equiv e₁ e₂ = normalize e₁ ≟ normalize e₂

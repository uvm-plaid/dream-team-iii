module Lang.JSDP.Equivalence where

import Lib

import Lang.JSDP.Syntax

data Atom = Lit 𝔹 | Var ℕ
  deriving (Eq,Ord,Show)

data NF = 𝑃 (𝐿 Atom)
  deriving (Eq,Ord,Show)

-- [!!] TODO
normalizeR 
  ∷ Exp              -- ^ The JSDP expression
  → ℕ                -- ^ The next unused number to assign to the next unseen variable
  → (𝕊 ⇰ ℕ)          -- ^ The map of existing assignments from variables to numbers
  → NF ∧ ℕ ∧ (𝕊 ⇰ ℕ) -- ^ The normalized expression, along with updated unused number
                     --   and variable mapping
normalizeR e n varmap = undefined

normalize ∷ Exp → NF
normalize e = π₁ $ π₁ $ normalizeR e (nat 0) empty𝐷

equiv ∷ Exp → Exp → 𝔹
equiv e₁ e₂ = normalize e₁ ≟ normalize e₂

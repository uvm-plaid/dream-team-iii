module Lang.JSDP.Syntax where

import Lib

-- Join-Semilattice w/Directed Products (JSDP) language
-- e.g., `(1 ⊔ x) ⋉ (y ⋉ z)` is an expression in this language
--
-- e ⩴ 0 | 1 | 𝑥 | e ⊔ e | e ⋉ e
--
-- laws:
--          0 ⊔ e = e                      [⊔-unit]
--        e₁ ⊔ e₂ = e₂ ⊔ e₁                [⊔-symmetry]
-- (e₁ ⊔ e₂) ⊔ e₃ = e₁ ⊔ (e₂ ⊔ e₃)         [⊔-associativity]
--          e ⊔ e = e                      [⊔-idempotent]
--
--          1 ⋉ e = e                      [⋉-left-unit]
--          e ⋉ 1 = e                      [⋉-right-unit]
-- (e₁ ⋉ e₂) ⋉ e₃ = e₁ ⋉ (e₂ ⋉ e₃)         [⋉-associativity]
-- e₁ ⋉ (e₂ ⊔ e₃) = (e₁ ⋉ e₂) ⊔ (e₁ ⋉ e₃)  [⋉-left-distributivity]
-- (e₁ ⊔ e₂) ⋉ e₃ = (e₁ ⋉ e₃) ⊔ (e₂ ⋉ e₃)  [⋉-right-distributivity]

-- e.g.:
--   (x ⊔ y) ⊔ 0 = (x ⊔ y)                 [via ⊔-unit and ⊔-symmetry]

data Exp =
    Lit 𝔹
  | Var 𝕊
  | Join Exp Exp
  | DProd Exp Exp
  deriving (Eq,Ord,Show)

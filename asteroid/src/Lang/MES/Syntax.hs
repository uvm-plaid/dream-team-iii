module Lang.MES.Syntax where

import Lib

-- Monadic Expressions w/Sums (MES) language
-- e.g., `(ret(z) ⊞ X) ≫= x.(ret(x) ≫= x.ret(x))` is an expression in this language
--
-- e ⩴ ret(e) | zero | 𝑥 | e ⊞ e | e ≫= x.e
--
-- laws:
--             zero ⊞ e = e                        [⊞-unit]
--              e₁ ⊞ e₂ = e₂ ⊞ e₁                  [⊞-symmetry]
--       (e₁ ⊞ e₂) ⊞ e₃ = e₁ ⊞ (e₂ ⊞ e₃)           [⊞-associativity]
--                e ⊞ e = e                        [⊞-idempotent]
--
--      ret(e₁) ≫= x.e₂ = let x ≔ e₁ in e₂         [≫=-left-unit]
--       e₁ ≫= x.ret(x) = e₁                       [≫=-right-unit]
-- (e₁ ≫= x.e₂) ≫= y.e₃ = e₁ ≫= x.(e₂ ≫= y.e₃)     [≫=-associativity]
-- e₁ ≫= x.(e₂ ⊞ e₃) = (e₁ ≫= x.e₂) ⊞ (e₁ ≫= x.e₃) [≫=-left-distributivity]
-- (e₁ ⊞ e₂) ≫= x.e₃ = (e₁ ≫= x.e₃) ⊞ (e₂ ≫= x.e₃) [≫=-right-distributivity]
--
-- e.g.:
--   (ret(x) ⊞ ret(y)) ⊞ zero = (ret(x) ⊞ ret(y))  [via ⊞-unit and ⊞-symmetry]

type Var = 𝕊
data Exp =
    Var Var
  | Lit ℕ
  | Ret Exp
  | Zero
  | Plus Exp Exp
  | Bind Exp Var Exp 
  -- [Bind e₁ x e₂] ≜ [e₁ ≫= x. e₂]
  -- e.g.,
  -- return 5 ≫= x. return x
  -- ≈
  -- return 5 ≫= y. return y
  --
  -- λ.λ.1 0
  --
  -- outside this term, y doesn't exist

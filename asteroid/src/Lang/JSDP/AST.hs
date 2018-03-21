module Lang.JSDP.AST where

import Lib

-- Join-Semilattice w/Directed Products (JSDP) language
-- e.g., `(1 ⊔ x) ⋉ (y ⋉ z)` is an expression in this language
-- e ⩴ 1 | 0 | 𝑥 | e ⊔ e | e ⋉ e
--
-- laws:
-- e ⊔ 0 = e
-- 0 ⊔ e = e
-- 1 ⋉ e = e
-- e ⋉ 1 = e
-- 0 ⋉ e = 0
-- e ⋉ 0 = 0
--
-- e.g.:
--   (return 1 <+> return 2) <+> none == (return 1 <+> return 2)

data Exp =
    Lit 𝔹
  | Var 𝕊
  | Join Exp Exp
  | DProd Exp Exp

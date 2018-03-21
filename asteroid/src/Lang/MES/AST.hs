module Lang.MES.AST where

import Lib

-- Monadic Expressions w/Sums (MES) language
-- e.g., `???` is an expression in this language
-- e ⩴ 1 | 0 | 𝑥 | e ⊞ e | e ≫= x. e

data Exp =
    Lit 𝔹
  | Var 𝕊
  | Join Exp Exp
  | DProd Exp Exp


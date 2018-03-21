module Lang.JSDP.AST where

import Lib

data Exp =
    Lit 𝔹
  | Var 𝕊
  | Join Exp Exp
  | DProd Exp Exp

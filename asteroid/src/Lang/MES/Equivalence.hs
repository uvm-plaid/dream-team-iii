module Lang.MES.Equivalence where

import Lib

import Lang.MES.Syntax

type Neutral = Var
data Product = Bind Neutral Var Product
type SumProd = 𝑃 Product 
data IfChain =
    Leaf SumProd
  | If SumProd IfChain IfChain
type NF = IfChain

unnormalize ∷ NF → Exp
unnormalize = undefined

normalize ∷ Exp → NF
normalize = undefined

module Lang.MES.Equivalence where

import Lib

import Lang.MES.Syntax

type Neutral = Name
data Product = BindNF Neutral Name Product
type SumProd = 𝑃 Product 
data IfChain =
    Leaf SumProd
  | IfNF SumProd IfChain IfChain
type NF = IfChain

unnormalize ∷ NF → Exp
unnormalize = undefined

normalize ∷ Exp → NF
normalize e = undefined

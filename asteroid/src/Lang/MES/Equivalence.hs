module Lang.MES.Equivalence where

import Lib

import Lang.MES.Syntax

type Neutral = Name
data Product = 
  ProductLeaf Neutral
  | BindNF Neutral Name Product
  deriving (Eq,Ord,Show)
type SumProd = 𝑃 Product 
data IfChain =
  IfLeaf SumProd
  | IfNF SumProd IfChain IfChain
  deriving (Eq,Ord,Show)
type NF = IfChain

unnormalize ∷ NF → Exp
unnormalize = undefined

normalize ∷ Exp → NF
normalize e = case e of
  Var x -> IfLeaf $ set𝐿 $ list $ [ProductLeaf x]
  Lit n -> undefined

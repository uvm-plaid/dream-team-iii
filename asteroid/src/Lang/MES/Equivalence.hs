module Lang.MES.Equivalence where

import Lib

import Lang.MES.Syntax

type Neutral = Name

--confused on what is the base data? JSDPC had String as it's base for leafdata
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

balanceIf ∷ SumProd → NF → NF → NF  --same as JSDPC balanceLink?
balanceIf x (IfLeaf y) (IfLeaf z)= IfNF x (IfLeaf y) (IfLeaf z)
balanceIf x (IfNF a b c) (IfLeaf y) = 
  case (a ⋚ x) of
  LT -> IfNF a (IfNF x b (IfLeaf y)) (IfNF x c (IfLeaf y))
  GT -> IfNF x (IfNF a b c) (IfLeaf y)
  EQ -> IfNF x b (IfLeaf y)

balanceIf x (IfLeaf y) (IfNF a b c) = 
  case (a ⋚ x) of
  LT -> IfNF a (IfNF x (IfLeaf y) b) (IfNF x (IfLeaf y) c)
  GT -> IfNF x (IfLeaf y) (IfNF a b c)
  EQ -> IfNF x (IfLeaf y) c

balanceIf x (IfNF a b c) (IfNF d e f) = 
  case (x ⋚ a, x ⋚ d, a ⋚ d) of
  (LT,LT,_) -> IfNF x (IfNF a b c) (IfNF d e f)
  (GT,_,LT) -> IfNF a (balanceIf x b (IfNF d e f))
                      (balanceIf x c (IfNF d e f))
  (_,GT,GT) -> IfNF d (balanceIf x (IfNF a b c) e)
                      (balanceIf x (IfNF a b c) f)
  (EQ,_,_) -> IfNF x b (IfNF d e f)
  (_,_,EQ) -> IfNF a (balanceIf x b e)
                     (balanceIf x c f)

retnf ∷ NF → NF
retnf x = undefined

plusnf ∷ NF → NF → NF
plusnf a b =  undefined

bindnf ∷ NF → Name → NF → NF
bindnf a n b =  undefined

ifnf ∷ NF → NF → NF → NF
ifnf (IfLeaf a) b c = balanceIf a b c
ifnf (IfNF x y z) a b = balanceIf x (ifnf y a b) (ifnf z a b)

normalize ∷ Exp → NF
normalize (Var n) =  undefined
normalize (Lit n) =  undefined
normalize (Ret exp) = retnf (normalize exp)
normalize Zero = undefined --empty set?
normalize (Plus e1 e2) = plusnf (normalize e1) (normalize e2)
normalize (Bind e1 n e2) = bindnf (normalize e1) n (normalize e2)
normalize (If e1 e2 e3) = ifnf (normalize e1) (normalize e2) (normalize e3)

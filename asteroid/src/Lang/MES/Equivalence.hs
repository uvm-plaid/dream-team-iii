module Lang.MES.Equivalence where

import Lib

import Lang.MES.Syntax

data Neutral = NName Name | NLit ℕ
  deriving (Eq,Ord,Show)

--confused on what is the base data? JSDPC had String as it's base for leafdata
data Product =
  ProductLeaf Neutral
  | BindNF Neutral Name Product       -- it's bad if we have (bind A x (return x))
                                      -- because we want it to be (A)
                                      -- however (bind A x (return 1)) is OK
  | ReturnNF Product
  -- get rid of this now?
  -- | ReturnBindNF Neutral Name Product 
                                      -- == returnbind n x p == return (bind n x p)
                                      -- return x == {x}
                                      -- A ≔ {1,2,3}
                                      -- λx.B = λx.{x+10,x+20}
                                      -- bind A (λx.B) = {11,12,13,21,22,23}
                                      -- bind A (λx.B) == {b | a ∈ A and b ∈ [x↦a]B}
                                      -- return (bind A x B)
                                      -- ≈
                                      -- { {b | a ∈ A and b ∈ [x↦a]B} }
                                      --
                                      -- [left unit]: (return a) ≫= x . B == [x↦a]B
                                      -- left unit law in sets
                                      -- c ∈ ℕ
                                      -- B ∈ ℘(ℕ) with one free variable x
                                      -- {b | a ∈ {c} and b ∈ [x↦a]B}
                                      -- ==
                                      -- {b | b ∈ [x↦c]B}
                                      -- ==
                                      -- [x↦c]B
                                      --
                                      -- [right unit]: A ≫= x. return x == A
                                      -- right unit law in sets
                                      -- {b | a ∈ X and b ∈ [x↦a]{x}}
                                      -- ==
                                      -- {b | a ∈ X and b ∈ {a}}
                                      -- == 
                                      -- {a | a ∈ X}
                                      -- ==
                                      -- X
                                      --
                                      -- [assoc]: (A ≫= x. B) ≫= y. C == A ≫= x. (B ≫= y. C)
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

retp ∷ Product → Product
retp (ProductLeaf n) = ReturnNF (ProductLeaf n)
retp (BindNF n x p) = ReturnNF (BindNF n x p)

retnf ∷ NF → NF
retnf (IfLeaf sp) = IfLeaf (map𝑃 retp sp)
retnf (IfNF sp nf₁ nf₂) = IfNF sp (retnf nf₁) (retnf nf₂)

plusnfL ∷ SumProd → NF → NF
plusnfL s1 (IfLeaf s2) = IfLeaf (s1 ∪ s2)
plusnfL s (IfNF x y z) = IfNF x (plusnfL s y) (plusnfL s z)

plusnf ∷ NF → NF → NF
plusnf (IfLeaf s1) nf2 = plusnfL s1 nf2
plusnf (IfNF x y z) n2 = balanceIf x (plusnf y n2) (plusnf z n2)

-- [!!] homework
-- use same strategy as plusnf
bindnfL ∷ SumProd → Name → NF → NF
bindnfL s1 n (IfLeaf s2) = undefined
bindnfL s1 n (IfNF x y z) = balanceIf x (bindnfL s1 n y) (bindnfL s1 n z)

bindnf ∷ NF → Name → NF → NF
bindnf (IfLeaf s1) n b = bindnfL s1 n b
bindnf (IfNF x y z) n b = balanceIf x (bindnf y n b) (bindnf z n b)

bindnfProd ∷ Product → Name → Product → NF
bindnfProd (ReturnNF a) x p = IfLeaf $ single𝑃 $ substProdramy x a p -- subst x a nf [left unit]
bindnfProd a x (ReturnNF (ProductLeaf (NName y))) | x == y = IfLeaf $ single𝑃  a

substProdramy ∷ Name → Product → Product → Product
substProdramy n p1 (ProductLeaf neu) = ProductLeaf $ substNeutralramy n p1 neu
substProdramy n1 p1 (BindNF neu n2 p2) = case (n1 == n2) of
  True -> BindNF neu n2 p2
  False -> BindNF neu n2 (substProdramy n1 p1 p2)
substProdramy n p1 (ReturnNF p2) = ReturnNF (substProdramy n p1 p2)

substNeutralramy ∷ Name → Product → Neutral → Neutral
substNeutralramy n1 p (NName n2) = case (n1 == n2) of
  True -> undefined
  False -> NName n2
substNeutralramy n p (NLit num) = NLit num

ifnf ∷ NF → NF → NF → NF
ifnf (IfLeaf a) b c = balanceIf a b c
ifnf (IfNF x y z) a b = balanceIf x (ifnf y a b) (ifnf z a b)

zeronf ∷ NF
zeronf = IfLeaf $ empty𝑃

varnf ∷ Name → NF
varnf x = IfLeaf $ single𝑃 $ ProductLeaf $ NName x

litnf ∷ ℕ → NF
litnf n = IfLeaf $ single𝑃 $ ProductLeaf $ NLit n

normalize ∷ Exp → NF
normalize (Var n) = varnf n
normalize (Lit n) =  litnf n
normalize (Ret exp) = retnf (normalize exp)
normalize Zero = zeronf
normalize (Plus e1 e2) = plusnf (normalize e1) (normalize e2)
normalize (Bind e1 n e2) = bindnf (normalize e1) n (normalize e2)
normalize (If e1 e2 e3) = ifnf (normalize e1) (normalize e2) (normalize e3)

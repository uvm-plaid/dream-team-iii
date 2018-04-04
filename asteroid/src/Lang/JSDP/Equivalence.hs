module Lang.JSDP.Equivalence where

import Lib

import Lang.JSDP.Syntax

-- NF is a "sum of products" representation.
type NF = 𝑃   (𝐿 𝕊)
--        ^    ^
-- set of sums |
--        list of products

-- e.g., here is a term:
-- x ⋉ (y ⊔ z) 
-- represented as an Exp as:
-- DProd (Var "x") (Join (Var "y") (Var "z))
-- but has no (direct) representation as an NF
-- ≈
-- (x ⋉ y) ⊔ (x ⋉ z)
-- represented as an Exp as:
-- Join (DProd (Var "x") (Var "y")) (DProd (Var "x") (Var "z"))
-- and as an NF:
-- { [x,y] , [x,z] }
--
-- its normal form (NF) is:
-- (x ⋉ y ⋈ z) ⊔ (a ⋉ b ⋉ c) ⊔ d ⊔ e
-- in list of lists form:
-- [ [x,y,z] , [a,b,c] , [d] , [e] ]
-- 
-- consider the same term with one more `e`
-- (x ⋉ y ⋈ z) ⊔ (a ⋉ b ⋉ c) ⊔ d ⊔ (e ⊔ e)
--                                  \____/
--                                  the same as e
--                                   
-- [ [x,y,z] , [a,b,c] , [d] , [e] , [e]]
-- because of the idempotency law:
--   x ⊔ x = x
-- this is equal to the term before:
-- (x ⋉ y ⋈ z) ⊔ (a ⋉ b ⋉ c) ⊔ d ⊔ e
-- i.e.,
-- [ [x,y,z] , [a,b,c] , [d] , [e] ]

-- A mapping back from normal forms to expressions:
unnormalize ∷ NF → Exp
unnormalize sps = 
  foldr𝐿 (Lit False) Join -- collapsing the outer list with joins, and unit element False
  $ map𝐿 (foldr𝐿 (Lit True) DProd) -- collapsing the inner list with products, and unit element True
  $ map𝐿 (map𝐿 Var) 
  $ list𝑃 sps
-- sps ∷ 𝑃 (𝐿 𝕊)
-- list𝑃 sps ∷ 𝐿 (𝐿 𝕊)
-- Var ∷ 𝕊 → Exp
-- map𝐿 (map𝐿 Var) ∷ 𝐿 (𝐿 𝕊) → 𝐿 (𝐿 Exp)
-- map𝐿 (map𝐿 Var) $ list𝑃 sps ∷ 𝐿 (𝐿 Exp)
-- foldr𝐿 (Lit True) 

combine ∷ a → 𝐿 b → 𝐿 (a ∧ b)
combine _ Nil = Nil
combine x (y :& ys) = (x :* y) :& combine x ys

-- oh my god it works
-- e.g., cart [1,2] [3,4] == [(1,3),(1,4),(2,3),(2,4)]
cart ∷ 𝐿 𝕊 → 𝐿 𝕊 → 𝐿 (𝕊 ∧ 𝕊)
-- e.g., cart [] [3,4] == []
cart Nil _ = Nil
-- e.g., cart [1,2] [3,4] 
-- x == 1
-- xs == [2]
-- ys = [3,4]
-- assume that the recursive call does the right thing:
-- cart xs ys == cart [2] [3,4] == [(2,3),(2,4)]
-- goal is to turn [(2,3),(2,4)] into [(1,3),(1,4),(2,3),(2,4)]
-- so we need to prepend [(1,3),(1,4)]
cart (x :& xs) ys = 
  (combine x ys)  -- return [(1,3),(1,4)]
  ⧺ 
  (cart xs ys)    -- return [(2,3),(2,4)]

-- e.g., 
-- [[1,2],[3,4]] ⨳ [[5,6],[7,8]]
-- ==
-- [[1,2,5,6],[1,2,7,8],[3,4,5,6],[3,4,7,8]]
--
-- [X,Y] ⨳ [A,B]
-- ==
-- [XA,XB,YA,YB]
cartEX ∷ 𝐿 (𝐿 𝕊) → 𝐿 (𝐿 𝕊) → 𝐿 (𝐿 𝕊)
cartEX Nil _ = Nil
cartEX (x :& xs) ys = map𝐿 (\ y → x ⧺ y) ys ⧺ cartEX xs ys

-- [!!] TODO
-- it should be the case that `unnormalize (normalize e)` returns an
-- "equivalent" formula, modulo the laws shown in Syntax.hs.
normalize 
  ∷ Exp  -- ^ The JSDP expression
  → NF   -- ^ The normalized expression.
normalize e = case e of
  Lit b -> case b of
    True ->
        set [list []]
    False ->
        set []
  Var x -> set [list [x]]
  Join x y -> 
    (∪) (normalize x) (normalize y)
  DProd x y -> 
    set𝐿 $ cartWith (⧺) (list𝑃 (normalize x)) (list𝑃 (normalize y))
  

equiv ∷ Exp → Exp → 𝔹
equiv e₁ e₂ = normalize e₁ ≟ normalize e₂

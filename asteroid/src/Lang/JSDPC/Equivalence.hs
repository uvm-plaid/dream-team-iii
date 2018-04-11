module Lang.JSDPC.Equivalence where

import Lib

import Lang.JSDPC.Syntax

-- e.g., if(x){if(y){a}{b}}{if(z){d}{e}}
--          ^
--      no nesting
--
-- in constructor form
-- Link "x" (Link "y" (Leaf "a") (Leaf "b")) (Link "z" (Leaf "d") (Leaf "e"))
--
-- INVARIANT
-- The variables mentioned in the link chain should be in strictly ascending
-- order, that is:
-- 
--     if(x){if(y){a}{b}}{if(z){d}{e}}
--
-- is a valid IfChain because x < y and x < z. The following IfChain is
-- therefore invalid:
--
--     if(y){if(x){a}{b}}{if(z){d}{e}}
--
-- because y ≮ z
--
-- also, this chain is not valid either:
--
--     if(x){if(y){a}{b}}{if(x){d}{e}}
--
-- because x ≮ x

--data IfChain = 
--    Leaf 𝕊
--  | Link 𝕊 IfChain IfChain
--  deriving (Ord, Eq, Show)

data NF = 
    Leaf (𝑃 (𝐿 𝕊))
  | Link 𝕊 NF NF
  deriving (Eq,Ord,Show)

--instance Ord IfChain where
  --compare _ _ = LT  

-- !! complete unnormalize
-- NF is a "sum of products of if-chains" representation.
--type NF = 𝑃 (𝐿 IfChain)

unnormalize ∷ NF → Exp
unnormalize sps = undefined
  -- foldr𝐿 (Lit False) Join
  -- $ map𝐿 (foldr𝐿 (Lit True) DProd)
  -- $ map𝐿 (map𝐿 Var) 
  -- $ list𝑃 sps

joinnf ∷ NF → NF → NF
joinnf n1 n2 = case n1 of
  Leaf s1 ->
    case n2 of
      Leaf s2 -> --take the union of two leaves 
        Leaf (s1 ∪ s2)
      Link x2 y2 z2 -> --propagate the left leaf into the right link
        Link x2 (joinnf y2 (Leaf s1)) (joinnf z2 (Leaf s1))
  Link x y z --set the left link to take precedence, and propagate right link into it
    -> Link x (joinnf y n2) (joinnf z n2)


dprodnf ∷ NF → NF → NF
dprodnf n1 n2 = case n1 of
  Leaf s1 ->
    case n2 of
      Leaf s2 -> --we have two leaves, so we take the cartesian product of them
        Leaf $ set𝐿 $ cartWith (⧺) (list𝑃 s1) (list𝑃 s2)
      -- we have a leaf and a link. The guard of the link takes precedence,
      -- and we distribute the contents of the leaf to the rest of the Link
      Link x2 y2 z2 -> 
        Link x2 (joinnf y2 (Leaf s1)) (joinnf z2 (Leaf s1))
  -- if n1 is a link, it takes precedence
  -- TODO: have the order determined by which is less than the other
  Link x y z
    -> Link x (joinnf y n2) (joinnf z n2)
 
ifnf ∷ NF → NF → NF → NF
ifnf sps n1 n2 = case sps of
  Leaf g -> ifnfutil (list𝑃 g) n1 n2 
  Link n n1 n2 -> Link n n1 n2

ifnfutil ∷ 𝐿 (𝐿 𝕊) → NF → NF → NF
ifnfutil (l :& Nil) n1 n2 = ifnfprods l n1 n2
--was not sure what to do in this case; assumed that different lists within leaf should be joined
ifnfutil (l :& ls) n1 n2 = joinnf (ifnfprods l n1 n2) (ifnfutil ls n1 n2)

ifnfprods ∷ 𝐿 𝕊 → NF → NF → NF
ifnfprods (x :& Nil) n1 n2 = Link x n1 n2
ifnfprods (x :& xs) n1 n2 = dprodnf (Link x n1 n2) (ifnfprods xs n1 n2) 
--ifnf (Leaf (sps ∷ 𝑃 (𝐿 𝕊))) (tb ∷ NF) (fb ∷ NF) = undefined


-- !! complete normalize
normalize 
  ∷ Exp  -- ^ The JSDP expression
  → NF   -- ^ The normalized expression.
normalize e = case e of
  Lit b -> case b of
     True ->
         Leaf $ set [list []]
     False ->
         Leaf $ set []
  Var x ->
    Leaf $ set [list [x]] 
  Join x y -> 
    joinnf (normalize x) (normalize y)
  DProd x y ->
    dprodnf (normalize x) (normalize y)
  If x y z ->
    ifnf (normalize x) (normalize y) (normalize z)
    --case x of --Only works when the guard is a Var, need to implement ifnf
      --Var v -> Link v (normalize y) (normalize z)

equiv ∷ Exp → Exp → 𝔹
equiv = undefined
-- equiv e₁ e₂ = normalize e₁ ≟ normalize e₂

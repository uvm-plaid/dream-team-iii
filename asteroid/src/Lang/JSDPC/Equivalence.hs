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

type LeafData = 𝑃 (𝐿 𝕊) 
data NF = 
    Leaf LeafData
  -- if (x){y}{z} 
  -- where x is a LeafData (call unnormalizeLeafData
  -- and y and z are NF (call unnormalize recursively)
  -- so you'll get xun, yun, zun, and you just build (If xun yun zun)
  | Link LeafData NF NF 
  deriving (Eq,Ord,Show)

--instance Ord IfChain where
  --compare _ _ = LT  

-- !! complete unnormalize
-- NF is a "sum of products of if-chains" representation.
--type NF = 𝑃 (𝐿 IfChain)

unnormalizeLeafData ∷ LeafData → Exp
unnormalizeLeafData sps = 
  foldr𝐿 (Lit False) Join
  $ map𝐿 (foldr𝐿 (Lit True) DProd)
  $ map𝐿 (map𝐿 Var) 
  $ list𝑃 sps
  
-- !! assignment: write down some non-trivial examples to normalize and unnormalize...
unnormalize ∷ NF → Exp
unnormalize (Leaf ld) = unnormalizeLeafData ld
unnormalize (Link lf nf1 nf2) = If (unnormalizeLeafData lf) (unnormalize nf1) (unnormalize nf2)

balanceNF ∷ NF -> NF
balanceNF (Leaf x) = Leaf x
balanceNF (Link x y z) = balanceLink x y z

-- if(y){if(x){a}{b}}{c} balances to if(x){if(y){a}{c}}{if(y){b}{c}}
-- if(y){a}{if(x){b}{c}} balances to if(x){if(y){a}{b}}{if(y){a}{c}}

balanceLink ∷ LeafData → NF → NF → NF
balanceLink x (Leaf y) (Leaf z) = Link x (Leaf y) (Leaf z)
balanceLink x (Link a b c) (Leaf y) = 
    case (a ⋚ x) of
    LT  -> Link a (Link x b (Leaf y)) (Link x c (Leaf y))
    GT -> Link x (Link a b c) (Leaf y)
    EQ -> Link x b (Leaf y) 

balanceLink x (Leaf y) (Link a b c) = 
    case (a ⋚ x) of
    LT ->  Link a (Link x (Leaf y) b) (Link x (Leaf y) c)
    GT -> Link x (Leaf y) (Link a b c)
    EQ -> Link x (Leaf y) c

balanceLink x (Link a b c) (Link d e f) = 
  case (x ⋚ a,x ⋚ d,a ⋚ d) of
    (LT,LT,_) -> 
      -- we have x < a 
      --         x < d
      Link x (Link a b c) (Link d e f)
    (GT,_,LT) -> 
      -- we have a < x
      --         a < d
      Link a (balanceLink x b (Link d e f))
             (balanceLink x c (Link d e f))
    (_,GT,GT) -> 
      -- we have d < x
      --         d < a
      Link d (balanceLink x (Link a b c) e)
             (balanceLink x (Link a b c) f)
    (EQ,_,_) ->
      -- we have x = a
      -- must be that d > x
      Link x b (Link d e f)
    (_,EQ,_) ->
      -- we have x = d
      -- must be that a > x
      Link x (Link a b c) f
    (_,_,EQ) ->
      -- we have a = d
      -- must be that x > a
      -- if(x){if(a){b}{c}}{if(a){e}{f}}
      -- if(a){if(x){b}{e}}{if(x){c}{f}}
      Link a (balanceLink x b e)
             (balanceLink x c f)
    (_,_,_) -> error "impossible"

checkInvariant ∷ NF → 𝔹
checkInvariant (Leaf _) = True
checkInvariant (Link _x (Leaf _y) (Leaf _z)) = True
checkInvariant (Link x (Link a b c) (Leaf _y)) = x < a ⩓ checkInvariant (Link a b c)
checkInvariant (Link x (Leaf _y) (Link a b c)) = x < a ⩓ checkInvariant (Link a b c)
checkInvariant (Link x (Link a b c) (Link d e f)) = 
  x < a 
  ⩓ x < d 
  ⩓ checkInvariant (Link a b c) 
  ⩓ checkInvariant (Link d e f)

joinnfL ∷ LeafData → NF → NF
joinnfL s1 (Leaf s2) = Leaf (s1 ∪ s2)
joinnfL s (Link x y z) = Link x (joinnfL s y) (joinnfL s z)

joinnf ∷ NF → NF → NF
joinnf (Leaf s1) nf2 = joinnfL s1 nf2
joinnf (Link x y z) n2 = balanceLink x (joinnf y n2) (joinnf z n2)

dprodnfL ∷ LeafData → NF → NF
dprodnfL s1 (Leaf s2) = Leaf (set𝐿 (cartWith (⧺) (list𝑃 s1) (list𝑃 s2)))
dprodnfL s (Link x y z) = Link x (dprodnfL s y) (dprodnfL s z)

dprodnf ∷ NF → NF → NF
dprodnf (Leaf s1) nf2 = dprodnfL s1 nf2
dprodnf (Link x y z) n2 = balanceLink x (dprodnf y n2) (dprodnf z n2)

-- ifnf n1 n2 n3
-- if we assume n1,n2,n3 balanced, then this returns a balanced tree
ifnf ∷ NF → NF → NF → NF
ifnf (Leaf g) n1 n2 = balanceLink g n1 n2
-- if(if(x){y}{z}){a}{b} normalizes to if(x){if(y){a}{b}}{if(z){a}{b}}
ifnf (Link x y z) a b = balanceLink x (ifnf y a b) (ifnf z a b) 

-- a ⊔ b == if a then true else b
-- a ⋉ b == if a then b else false


-- !! complete normalize
-- it must be guaranteed that normalize will return a balanced tree
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

equiv ∷ Exp → Exp → 𝔹
equiv e1 e2 = (normalize e1) == (normalize e2)

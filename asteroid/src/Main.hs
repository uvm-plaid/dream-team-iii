module Main where

import Asteroid


main ∷ IO ()
main = do
  -- println $ show𝕊 $ foldr𝐿 Nil (:&) $ list [1,2,3,4]
  -- println $ show𝕊 $ Join (Lit False) (DProd (Lit True) (Var "x"))
  println $ show𝕊 $ unnormalize $ set [list ["x","y"],list ["z"],list []]

  println $ show𝕊 $ unnormalize $ set [list ["x","y"]]
  
  println $ show𝕊 $ unnormalize $ set [list ["x"], list ["y"]]
  -- > Join (DProd (Var "x") (DProd (Var "y") (Lit True))) (Join (DProd (Var "z") (Lit True)) (Lit False))
  -- ≈ (x ⋉ y ⋉ True) ⊔ (z ⋉ True) ⊔ False
  -- ≈ (x ⋉ y) ⊔ z
  -- x ⋉ True == x
  -- x ⊔ False == x
  --
  -- what is the normal form for this term: `False`
  println $ show𝕊 $ unnormalize $ set []
  -- either x₁ ⊔ ⋯ ⊔ xₙ for n = 0
  -- or just False
  -- or {} in normal form
  --
  -- what is the normal form for this term: `True`
  println $ show𝕊 $ unnormalize $ set [list []]
  -- either x₁ ⋉ ⋯ ⋉ xₙ for n = 0
  -- or Just True
  -- or {[]}
  --
  -- think of (True ⋉ False) ⊔ (True ⋉ y)
  -- as       (1    × 0    ) + (1    × y)
  -- (we are not assuming (y ⋉ z == z ⋉ y) although this is true for arithmetic)

  foldExamples

example1 ∷ ℤ
example1 = fold𝐿 (-1) (⩏) $ list [1,2,4]

foldExamples ∷ IO ()
foldExamples = do
  -- some fold examples

  -- the "left fold", which we just call "fold" (the "𝐿" in "fold𝐿" means
  -- "list", as in "fold for lists").
  
  -- computing the max element in a list can be done with fold𝐿
  let max ∷ 𝐿 ℤ → ℤ
      max = fold𝐿 0 (⩏)
  
  -- let reduce ∷ (a → a → a) → 𝐿 a → a
  --     reduce f Nil = error "empty list"
  --     reduce f (x :& Nil) = x
  --     reduce f (x :& y :& xs) = f x (reduce f (y :& xs))

  println $ show𝕊 $ max $ list []
  println $ show𝕊 $ max $ list [1,2,3,4]
  println $ show𝕊 $ max $ 1 :& 2 :& 3 :& 4 :& Nil
  println $ show𝕊 $ (((1 ⩏ 2) ⩏ 3) ⩏ 4) ⩏ 0

  -- fold𝐿 BASECASE × [1,2,3,4,5]
  -- == 
  -- ((((1 × 2) × 3) × 4) × 5) × BASECASE

  -- if the first argument is `None` return the second argument, and if it is
  -- `Some x`, keep it.
  let firstSome ∷ 𝑂 a → 𝑂 a → 𝑂 a
      firstSome None xO = xO
      firstSome (Some x) _ = Some x

  -- find the last element in a list of options that is not `None`
  let lastSomeInList ∷ 𝐿 (𝑂 a) → 𝑂 a
      lastSomeInList = fold𝐿 None firstSome

  -- the "right fold", which we call "foldr", or "foldr𝐿" for the right fold
  -- over lists.

  -- find the first element in a list of options that is not `None`
  let firstSomeInList ∷ 𝐿 (𝑂 a) → 𝑂 a
      firstSomeInList = foldr𝐿 None firstSome

  println $ show𝕊 $ lastSomeInList $ list [None,Some 1, None,Some 2,None]
  println $ show𝕊 $ firstSomeInList $ list [None,Some 1, None,Some 2,None]

  -- don't worry about foldk𝐿. It's just a trick used to define `foldr` in
  -- terms of `fold`. This means that for a fancy data structure (like a tree)
  -- you only need to define `fold` (the left fold) and you can get `foldr`
  -- (the right fold) "for free" through `foldk`.

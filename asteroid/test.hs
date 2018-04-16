module Main where

import Asteroid

main ∷ IO()
main = do
  println $ show𝕊 $ foldr𝐿 Nil (:&) $ list [1, 2, 3, 4]
  println $ show𝕊 $ Join (Lit False) (DProd (Lit True) (Var "x"))
  println $ show𝕊 $ unnormalize $ (Leaf $ (set []))  
  println $ show𝕊 $ unnormalize $ (Leaf $ set [list []])  
  --println $ show𝕊 $ unnormalize $ Leaf $ set [list ["x", "y"], list ["z"]]

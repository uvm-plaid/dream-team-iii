module Main where

import Asteroid

main ∷ IO()
main = do
--  println $ show𝕊 $ foldr𝐿 Nil (:&) $ list [1, 2, 3, 4]
--  println $ show𝕊 $ Join (Lit False) (DProd (Lit True) (Var "x"))
--  println $ show𝕊 $ unnormalize $ (Leaf $ (set []))  
--  println $ show𝕊 $ unnormalize $ Leaf $ set [list ["x", "y"], list ["z"]]

  -- balanceLink 1
  -- if(x) { if(a) {b} {c} } {y}  = if(a) { if(x) {b} {y} } { if(x) {c} {y} }
  println $ show𝕊 $ equiv 
    (If (Var "x") (If (Var "a") (Var "b") (Var "c")) (Var "y"))
    (If (Var "a") (If (Var "x") (Var "b") (Var "y")) (If (Var "x") (Var "c") (Var "y")))
  println $ show𝕊 $ checkInvariant $ normalize 
    (If (Var "x") (If (Var "a") (Var "b") (Var "c")) (Var "y"))

  --balanceLink 2
  -- if(x) {y} { if(a) {b} {c} }  = if(a) { if(x) {y} {b} } { if(x) {y} {c} }
  println $ show𝕊 $ equiv 
    (If (Var "x") (Var "y") (If (Var "a") (Var "b") (Var "c")))
    (If (Var "a") (If (Var "x") (Var "y") (Var "b")) (If (Var "x") (Var "y") (Var "c")))
  println $ show𝕊 $ checkInvariant $ normalize 
    (If (Var "x") (Var "y") (If (Var "a") (Var "b") (Var "c")))

  -- balanceLink 3 case 1
  -- if(x) {y} { if(a) {b} {c} }  = if(a) { if(x) {y} {b} } { if(x) {y} {c} }
  println $ show𝕊 $ equiv 
    (If (Var "a") (If (Var "b") (Var "c") (Var "d")) (If (Var "e") (Var "f") (Var "g")))
    (If (Var "a") (If (Var "b") (Var "c") (Var "d")) (If (Var "e") (Var "f") (Var "g")))
  println $ show𝕊 $ checkInvariant $ normalize 
    (If (Var "a") (If (Var "b") (Var "c") (Var "d")) (If (Var "e") (Var "f") (Var "g")))

  --balanceLink 3 case 2. no recursion needed
  println $ show𝕊 $ equiv 
    (If (Var "b") (If (Var "a") (Var "c") (Var "d")) (If (Var "e") (Var "f") (Var "g")))
    (If (Var "a") (If (Var "b") (Var "c") (If (Var "e") (Var "f") (Var "g"))) (If (Var "b") (Var "d") (If (Var "e") (Var "f") (Var "g"))))
  println $ show𝕊 $ checkInvariant $ normalize 
    (If (Var "b") (If (Var "a") (Var "c") (Var "d")) (If (Var "e") (Var "f") (Var "g")))
  
  --balanceLink 3 case 2. recursion needed
  println $ show𝕊 $ equiv 
    (If (Var "x") (If (Var "a") (Var "c") (Var "d")) (If (Var "e") (Var "f") (Var "g")))
    (If (Var "a") (If (Var "x") (Var "c") (If (Var "e") (Var "f") (Var "g"))) (If (Var "x") (Var "d") (If (Var "e") (Var "f") (Var "g"))))
  println $ show𝕊 $ checkInvariant $ normalize 
    (If (Var "x") (If (Var "a") (Var "c") (Var "d")) (If (Var "e") (Var "f") (Var "g")))

  --balanceLink 3 case 3. recursion
  println $ show𝕊 $ equiv 
    (If (Var "x") (If (Var "e") (Var "c") (Var "d")) (If (Var "a") (Var "f") (Var "g")))
    (If (Var "a") (If (Var "x") (If (Var "e") (Var "c") (Var "d")) (Var "f")) (If (Var "x") (If (Var "e") (Var "c") (Var "d")) (Var "g")))
  println $ show𝕊 $ checkInvariant $ normalize 
    (If (Var "x") (If (Var "e") (Var "c") (Var "d")) (If (Var "a") (Var "f") (Var "g")))
  
  --balanceLink 3 case 3. no recursion
  println $ show𝕊 $ equiv 
    (If (Var "b") (If (Var "e") (Var "c") (Var "d")) (If (Var "a") (Var "f") (Var "g")))
    (If (Var "a") (If (Var "b") (If (Var "e") (Var "c") (Var "d")) (Var "f")) (If (Var "b") (If (Var "e") (Var "c") (Var "d")) (Var "g")))
  println $ show𝕊 $ checkInvariant $ normalize 
    (If (Var "b") (If (Var "e") (Var "c") (Var "d")) (If (Var "a") (Var "f") (Var "g")))

  --balanceLink case 3 case 4
  println $ show𝕊 $ equiv
    (If (Var "a") (If (Var "a") (Var "b") (Var "c")) (If (Var "d") (Var "e") (Var "f")))
    (If (Var "a") (Var "b") (If (Var "d") (Var "e") (Var "f")))
  println $ show𝕊 $ checkInvariant $ normalize 
    (If (Var "a") (If (Var "a") (Var "b") (Var "c")) (If (Var "d") (Var "e") (Var "f")))

  --balanceLink case 3 case 5
  println $ show𝕊 $ equiv
    (If (Var "a") (If (Var "x") (Var "y") (Var "z")) (If (Var "a") (Var "b") (Var "c")))
    (If (Var "a") (If (Var "x") (Var "y") (Var "z")) (Var "c"))
  println $ show𝕊 $ checkInvariant $ normalize 
    (If (Var "a") (If (Var "x") (Var "y") (Var "z")) (If (Var "a") (Var "b") (Var "c")))

  --balanceLink case 3 case 6
  println $ show𝕊 $ equiv
    (If (Var "x") (If (Var "a") (Var "b") (Var "c")) (If (Var "a") (Var "e") (Var "f")))
    (If (Var "a") (If (Var "x") (Var "b") (Var "e")) (If (Var "x") (Var "c") (Var "f")))
  println $ show𝕊 $ checkInvariant $ normalize 
    (If (Var "x") (If (Var "a") (Var "b") (Var "c")) (If (Var "a") (Var "e") (Var "f")))

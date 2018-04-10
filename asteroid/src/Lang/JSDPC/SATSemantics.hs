module Lang.JSDPC.SATSemantics where

import Lib

import Lang.JSDPC.Syntax

-- !! complete interpret
interpret ∷ Exp → 𝕊 ⇰ 𝔹 → 𝑂 𝔹
interpret e env = case e of
 Lit b -> return b
 Var x -> env # x
 Join x y -> do
   bx ← interpret x env 
   by ← interpret y env
   return $ bx ⩔ by
 DProd x y -> do
   bx ← interpret x env 
   by ← interpret y env
   return $ bx ⩓ by
 If e₁ e₂ e₃ → undefined

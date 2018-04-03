module Lang.JSDP.SATSemantics where

import Lib

import Lang.JSDP.Syntax

-- [!!] TODO

boop ∷ 𝕊 → 𝕊
boop s = undefined

interpret ∷ Exp → 𝕊 ⇰ 𝔹 → 𝔹
interpret e env = case e of
 Lit b -> b
 Var x -> unpackOptional $ lookup𝐷 x env
 Join x y -> (interpret x env) ⩔ (interpret y env)
 DProd x y -> (interpret x env) ⩓ (interpret y env)

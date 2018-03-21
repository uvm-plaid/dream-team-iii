module Lang.JSDP.Parse where

import Lib

import Lang.JSDP.Syntax

parse ∷ Parser Exp
parse = undefined

-- ======= --
-- LIBRARY --
-- ======= --

type Parser a = 𝐿 ℂ → 𝐿 (a ∧ 𝐿 ℂ)

---------------
-- Low Level --
---------------

pEmpty ∷ Parser a
pEmpty = undefined

pUnion ∷ Parser a → Parser a → Parser a
pUnion p₁ p₂ = undefined

pUnions ∷ 𝐿 (Parser a) → Parser a
pUnions ps = undefined

pFail ∷ Parser a
pFail = undefined

pAdvance ∷ Parser (𝑂 ℂ)
pAdvance = undefined

pPluck ∷ Parser ℂ
pPluck = undefined

pEnd ∷ Parser ()
pEnd = undefined

----------------
-- High Level --
----------------

pFinal ∷ Parser a → Parser a
pFinal p = undefined

pAny ∷ Parser ℂ
pAny = undefined

pShaped ∷ (ℂ → 𝑂 a) → Parser a
pShaped f = undefined

pSatisfies ∷ (ℂ → 𝔹) → Parser ℂ
pSatisfies f = undefined

pLit ∷ ℂ → Parser ℂ
pLit c = undefined

pWord ∷ 𝕊 → Parser 𝕊
pWord = undefined

pOptional ∷ Parser a → Parser (𝑂 a)
pOptional p = undefined

pMany ∷ Parser a → Parser (𝐿 a)
pMany p = undefined

pOneOrMore ∷ Parser a → Parser (𝐿 a)
pOneOrMore p = undefined

pManySepBy ∷ Parser () → Parser a → Parser (𝐿 a)
pManySepBy pⁱ pˣ = undefined

-------------
-- Helpers --
-------------

pLParen ∷ Parser ()
pLParen = undefined

pRParen ∷ Parser ()
pRParen = undefined

pDigit ∷ Parser ℂ
pDigit = undefined

pNatural ∷ Parser ℕ
pNatural = undefined

pInteger ∷ Parser ℤ
pInteger = undefined

pDouble ∷ Parser 𝔻
pDouble = undefined

pNumber ∷ Parser (ℤ ∨ 𝔻)
pNumber = undefined

pLetter ∷ Parser ℂ
pLetter = undefined

pWhitespace ∷ Parser 𝕊
pWhitespace = undefined

pOptionalWhitespace ∷ Parser ()
pOptionalWhitespace = undefined

pSurroundedBy ∷ Parser () → Parser () → Parser a → Parser a
pSurroundedBy pˡ pʳ p = undefined

pSurrounded ∷ Parser () → Parser a → Parser a
pSurrounded pᵇ p = undefined


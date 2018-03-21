module Lang.JSDP.Pretty where

import Lib

import Lang.JSDP.Syntax

pretty ∷ Exp → Doc
pretty = undefined

-- ======= --
-- LIBRARY --
-- ======= --

data Mode = Flat | Break

data PrettyEnv = PrettyEnv
  { nesting ∷ ℕ
  , precedence ∷ ℕ
  , bumped ∷ 𝔹
  , mode ∷ Mode
  }

env₀ ∷ PrettyEnv
env₀ = PrettyEnv
  { nesting = nat 0
  , precedence = nat 0
  , bumped = False
  , mode = Break
  }

data PrettyState = PrettyState
  { column ∷ ℕ }

state₀ ∷ PrettyState
state₀ = PrettyState
  { column = nat 0 
  }

type Doc = PrettyEnv → PrettyState → (𝕊 ∧ PrettyState)

---------------
-- Low Level --
---------------

raw ∷ 𝕊 → Doc
raw s = undefined

word ∷ 𝕊 → Doc
word s = undefined

newline ∷ Doc
newline = undefined

----------------
-- High Level --
----------------

ppSpace ∷ ℕ → Doc
ppSpace n = undefined

ppNewline ∷ Doc
ppNewline = undefined

ppText ∷ 𝕊 → Doc
ppText = undefined

ppIfFlat ∷ Doc → Doc → Doc
ppIfFlat flatAction breakAction = undefined

ppFlat ∷ Doc → Doc
ppFlat d = undefined

ppBreak ∷ Doc → Doc
ppBreak d = undefined

ppGroup ∷ Doc → Doc
ppGroup d = undefined

ppNest ∷ ℕ → Doc → Doc
ppNest n d = undefined

ppAlign ∷ Doc → Doc
ppAlign d = undefined

runDoc ∷ Doc → 𝕊
runDoc d = π₁ $ d env₀ state₀

module Lib.Core 
  ( module Lib.Core
  , module Prelude
  ) where

-- These are the only things we import (and re-export) from the standard
-- Prelude:
import Prelude
  ( Bool(..)
  , ($)
  , undefined
  , otherwise
  , IO
  , Eq((==))
  , Ord(compare)
  , Show(show)
  , Ordering(..))

import qualified Prelude as HS
import qualified Numeric.Natural as HS
import qualified System.Exit as HS
import qualified System.IO.Unsafe as HS

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Set as Set
import qualified Data.Map as Map

infixr 1 ≫=

infixr 2 ⇰,↦

infixl 3 ⩔,⩏,∪,∖,⩊,⧺
infixl 4 ⩓,⩎,∩

infix  5 ≟,≠,⋚,≤,≥,<,>,∈,⊆,⋿
infixl 6 ∘

infixr 8 :&

-- Functions

id ∷ a → a
id x = x

const ∷ a → b → a
const x _ = x

(∘) ∷ (b → c) → (a → b) → a → c
(g ∘ f) x = g (f x)

-- Booleans

type 𝔹 = HS.Bool

not ∷ 𝔹 → 𝔹
not True = False
not False = True

(⩓) ∷ 𝔹 → 𝔹 → 𝔹
True ⩓ x = x
x ⩓ True = x
False ⩓ False = False

(⩔) ∷ 𝔹 → 𝔹 → 𝔹
False ⩔ x = x
x ⩔ False = x
True ⩔ True = True

-- Ordering

(≟) ∷ (Eq a) ⇒ a → a → 𝔹
(≟) = (HS.==)

(≠) ∷ (Eq a) ⇒ a → a → 𝔹
(≠) = (HS./=)

(⋚) ∷ (Ord a) ⇒ a → a → Ordering
(⋚) = compare

(≤) ∷ (Ord a) ⇒ a → a → 𝔹
x ≤ y = case x ⋚ y of {LT → True;EQ → True;GT → False}

(≥) ∷ (Ord a) ⇒ a → a → 𝔹
x ≥ y = case x ⋚ y of {LT → False;EQ → True;GT → True}

(<) ∷ (Ord a) ⇒ a → a → 𝔹
(<) = (HS.<)

(>) ∷ (Ord a) ⇒ a → a → 𝔹
(>) = (HS.>)

(⩎) ∷ (Ord a) ⇒ a → a → a
x ⩎ y 
  | x ≤ y = x
  | otherwise = y

(⩏) ∷ (Ord a) ⇒ a → a → a
x ⩏ y 
  | x ≤ y = y
  | otherwise = x

-- Integers

type ℤ = HS.Integer

-- RebindableSyntax
fromInteger ∷ HS.Integer → ℤ
fromInteger = HS.fromIntegral

-- RebindableSyntax
negate ∷ ℤ → ℤ
negate = HS.negate

-- Natural numbers

type ℕ = HS.Natural

nat ∷ ℤ → ℕ
nat = HS.fromIntegral

-- Doubles

type 𝔻 = HS.Double

-- RebindableSyntax
fromRational ∷ HS.Rational → 𝔻
fromRational = HS.fromRational

blah ∷ ℕ
blah = nat 2

-- characters
type ℂ = HS.Char

-- RebindableSyntax
fromString ∷ [ℂ] → 𝕊
fromString = Text.pack

-- Sums
data a ∨ b = Inl a | Inr b
  deriving (Eq,Ord,Show)

-- Products
data a ∧ b = a :* b
  deriving (Eq,Ord,Show)

π₁ ∷ a ∧ b → a
π₁ (x :* _) = x

π₂ ∷ a ∧ b → b
π₂ (_ :* y) = y

-- Options
data 𝑂 a = None | Some a
  deriving (Eq,Ord,Show)

-- Lists (non-lazy)
data 𝐿 a = Nil | a :& 𝐿 a
  deriving (Eq,Ord)

instance (Show a) ⇒ Show (𝐿 a) where show = show ∘ toLL


-- to lazy list
toLL ∷ 𝐿 a → [a]
toLL Nil = []
toLL (x :& xs) = x : toLL xs

-- e.g., list [1,2,3]
list ∷ [a] → 𝐿 a
list [] = Nil
list (x : xs) = x :& list xs

(⧺) ∷ 𝐿 a → 𝐿 a → 𝐿 a
xs ⧺ ys = foldr𝐿 ys (:&) xs

fold𝐿 ∷ b → (a → b → b) → 𝐿 a → b
fold𝐿 i _ Nil = i
fold𝐿 i f (x :& xs) = fold𝐿 (f x i) f xs

foldWith𝐿 ∷ 𝐿 a → (a → b → b) → b → b
foldWith𝐿 xs f i = fold𝐿 i f xs

foldFrom𝐿 ∷ 𝐿 a → b → (a → b → b) → b
foldFrom𝐿 xs i f = fold𝐿 i f xs

foldk𝐿 ∷ b → (a → (b → b) → b → b) → 𝐿 a → b
foldk𝐿 i f xs₀ = fold𝐿 id f xs₀ i

foldkWith𝐿 ∷ 𝐿 a → (a → (b → b) → b → b) → b → b
foldkWith𝐿 xs f i = foldk𝐿 i f xs

foldkFrom𝐿 ∷ 𝐿 a → b → (a → (b → b) → b → b) → b
foldkFrom𝐿 xs i f = foldk𝐿 i f xs

foldr𝐿 ∷ b → (a → b → b) → 𝐿 a → b
foldr𝐿 i f = foldk𝐿 i $ \ x k → k ∘ f x

foldrWith𝐿 ∷ 𝐿 a → (a → b → b) → b → b
foldrWith𝐿 xs f i = foldr𝐿 i f xs

foldrFrom𝐿 ∷ 𝐿 a → b → (a → b → b) → b
foldrFrom𝐿 xs i f = foldr𝐿 i f xs

map𝐿 ∷ (a → b) → 𝐿 a → 𝐿 b
map𝐿 f = foldr𝐿 Nil $ \ x ys → f x :& ys

prefixed𝐿 ∷ a → 𝐿 a → 𝐿 a
prefixed𝐿 _ Nil = Nil
prefixed𝐿 i (x :& xs) = i :& x :& prefixed𝐿 i xs

inbetween𝐿 ∷ a → 𝐿 a → 𝐿 a
inbetween𝐿 _ Nil = Nil
inbetween𝐿 i (x :& xs) = x :& prefixed𝐿 i xs

-- Strings
-- if there is something you need from the text package module Data.Text:
--
-- https://hackage.haskell.org/package/text-1.2.3.0/docs/Data-Text.html
--
-- then add it, or ping me on Slack.

type 𝕊 = Text.Text

error ∷ 𝕊 → a
error = HS.error ∘ Text.unpack

empty𝕊 ∷ 𝕊
empty𝕊 = Text.empty

single𝕊 ∷ ℂ → 𝕊
single𝕊 = Text.singleton

(⩊) ∷ 𝕊 → 𝕊 → 𝕊
(⩊) = Text.append

build𝕊 ∷ 𝐿 ℂ → 𝕊
build𝕊 cs = Text.pack (toLL cs)

chars ∷ 𝕊 → 𝐿 ℂ
chars s = list (Text.unpack s)

show𝕊 ∷ (Show a) ⇒ a → 𝕊
show𝕊 = Text.pack ∘ Prelude.show

lower𝕊 ∷ 𝕊 → 𝕊
lower𝕊 = Text.toLower

ith ∷ ℕ → 𝕊 → ℂ
ith n s = Text.index s (HS.fromIntegral n)

upper𝕊 ∷ 𝕊 → 𝕊
upper𝕊 = Text.toUpper

isEmpty𝕊 ∷ 𝕊 → 𝔹
isEmpty𝕊 = Text.null

splitOn𝕊 ∷ 𝕊 → 𝕊 → 𝐿 𝕊
splitOn𝕊 i s = list $ Text.splitOn i s

length𝕊 ∷ 𝕊 → ℕ
length𝕊 = HS.fromIntegral ∘ Text.length

concat𝕊 ∷ 𝐿 𝕊 → 𝕊
concat𝕊 = foldr𝐿 empty𝕊 (⩊)

showCollection ∷ 𝕊 → 𝕊 → 𝕊 → (a → 𝕊) → 𝐿 a → 𝕊
showCollection l r i showA xs = concat𝕊 $ list
  [ l
  , concat𝕊 $ inbetween𝐿 i $ map𝐿 showA xs
  , r
  ]

-- Sets
type 𝑃 = Set.Set

empty𝑃 ∷ 𝑃 a
empty𝑃 = Set.empty

single𝑃 ∷ (Ord a) ⇒ a → 𝑃 a
single𝑃 = Set.singleton

insert𝑃 ∷ (Ord a) ⇒ a → 𝑃 a → 𝑃 a
insert𝑃 = Set.insert

(∈) ∷ (Ord a) ⇒ a → 𝑃 a → 𝔹
(∈) = Set.member

remove𝑃 ∷ (Ord a) ⇒ a → 𝑃 a → 𝑃 a
remove𝑃 = Set.delete

removeMin𝑃 ∷ (Ord a) ⇒ 𝑃 a → 𝑂 (a ∧ 𝑃 a)
removeMin𝑃 xs = case Set.minView xs of
  HS.Nothing → None
  HS.Just (x,xs') → Some (x :* xs')

removeMax𝑃 ∷ (Ord a) ⇒ 𝑃 a → 𝑂 (a ∧ 𝑃 a)
removeMax𝑃 xs = case Set.maxView xs of
  HS.Nothing → None
  HS.Just (x,xs') → Some (x :* xs')

(∪) ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝑃 a
(∪) = Set.union

(∩) ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝑃 a
(∩) = Set.intersection

(⊆) ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝔹
(⊆) = Set.isSubsetOf

(∖) ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝑃 a
(∖) = Set.difference

--cart ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝑃 a
--cart = Set.cartesianProduct

uniques ∷ (Ord a) ⇒ 𝐿 a → 𝐿 a
uniques xs₀ = π₁ $ foldrFrom𝐿 xs₀ (Nil :* empty𝑃) $ \ x (xs :* seen) →
  case x ∈ seen of
    True → xs :* seen
    False → (x :& xs) :* (insert𝑃 x seen)

showWith𝑃 ∷ (a → 𝕊) → 𝑃 a → 𝕊
showWith𝑃 showA = showCollection "{" "}" "," showA ∘ list𝑃

list𝑃 ∷ 𝑃 a → 𝐿 a
list𝑃 = list ∘ Set.toList

set𝐿 ∷ (Ord a) ⇒ 𝐿 a → 𝑃 a
set𝐿 = fold𝐿 empty𝑃 insert𝑃

set ∷ (Ord a) ⇒ [a] → 𝑃 a
set = set𝐿 ∘ list

-- Dictionaries
type (⇰) = Map.Map

empty𝐷 ∷ k ⇰ v
empty𝐷 = Map.empty

(↦) ∷ (Ord k) ⇒ k → v → k ⇰ v
(↦) = Map.singleton

insertWith𝐷 ∷ (Ord k) ⇒ (v → v → v) → k → v → k ⇰ v → k ⇰ v
insertWith𝐷 = Map.insertWith

insert𝐷 ∷ (Ord k) ⇒ k → v → k ⇰ v → k ⇰ v
insert𝐷 = insertWith𝐷 const

(#) ∷ (Ord k) ⇒ k ⇰ v → k → 𝑂 v
(#) kvs k = case Map.lookup k kvs of
  HS.Nothing → None
  HS.Just v → Some v

(#!) ∷ (Ord k) ⇒ k ⇰ v → k → v
(#!) = (Map.!)

(⋿) ∷ (Ord k) ⇒ k → k ⇰ v → 𝔹
k ⋿ kvs = case kvs # k of
  None → False
  Some _ → True

without𝐷 ∷ (Ord k) ⇒ k → k ⇰ v → 𝑂 (v ∧ (k ⇰ v))
without𝐷 k kvs = case kvs # k of
  None → None
  Some v → Some (v :* Map.delete k kvs)

remove𝐷 ∷ (Ord k) ⇒ k → k ⇰ v → k ⇰ v
remove𝐷 k kvs = case without𝐷 k kvs of
  None → kvs
  Some (_ :* kvs') → kvs'
  
removeMin𝐷 ∷ (Ord k) ⇒ k ⇰ v → 𝑂 ((k ∧ v) ∧ (k ⇰ v))
removeMin𝐷 kvs = case Map.minViewWithKey kvs of
  HS.Nothing → None
  HS.Just ((k,v),kvs') → Some ((k :* v) :* kvs')

removeMax𝐷 ∷ (Ord k) ⇒ k ⇰ v → 𝑂 ((k ∧ v) ∧ (k ⇰ v))
removeMax𝐷 kvs = case Map.maxViewWithKey kvs of
  HS.Nothing → None
  HS.Just ((k,v),kvs') → Some ((k :* v) :* kvs')

unionWith𝐷 ∷ (Ord k) ⇒ (v → v → v) → k ⇰ v → k ⇰ v → k ⇰ v
unionWith𝐷 = Map.unionWith

intersectWith𝐷 ∷ (Ord k) ⇒ (v → v → v) → k ⇰ v → k ⇰ v → k ⇰ v
intersectWith𝐷 = Map.intersectionWith

isSubdictBy𝐷 ∷ (Ord k) ⇒ (v → v → 𝔹) → k ⇰ v → k ⇰ v → 𝔹
isSubdictBy𝐷 = Map.isSubmapOfBy

minusWith𝐷 ∷ (Ord k) ⇒ (v → v → v) → k ⇰ v → k ⇰ v → k ⇰ v
minusWith𝐷 f = Map.differenceWith (\ k v → HS.Just (f k v))

keys𝐷 ∷ (Ord k) ⇒ k ⇰ v → 𝑃 k
keys𝐷 = Map.keysSet

restrict𝐷 ∷ (Ord k) ⇒ 𝑃 k → k ⇰ v → k ⇰ v
restrict𝐷 ks kvs = foldFrom𝐿 (list𝑃 ks) empty𝐷 $ \ k → case kvs # k of
  None → id
  Some v → insert𝐷 k v

purge𝐷 ∷ (Ord k) ⇒ 𝑃 k → k ⇰ v → k ⇰ v
purge𝐷 ks kvs₀ = foldFrom𝐿 (list𝑃 ks) kvs₀ remove𝐷

showWith𝐷 ∷ (k → 𝕊) → (v → 𝕊) → k ⇰ v → 𝕊
showWith𝐷 showK showV = showCollection "{" "}" "," showKV ∘ list𝐷
  where
    showKV (k :* v) = concat𝕊 $ list [showK k,"↦",showV v]

list𝐷 ∷ k ⇰ v → 𝐿 (k ∧ v)
list𝐷 = map𝐿 (\ (k,v) → k :* v) ∘ list ∘ Map.toList

dict𝐿 ∷ (Ord k) ⇒ 𝐿 (k ∧ v) → k ⇰ v
dict𝐿 = Map.fromList ∘ toLL ∘ map𝐿 (\ (k :* v) → (k,v))

dict ∷ (Ord k) ⇒ [(k,v)] → k ⇰ v
dict = dict𝐿 ∘ map𝐿 (\ (k,v) → k :* v) ∘ list

-- Monad

class Monad (m ∷ ★ → ★) where
  return ∷ a → m a
  (≫=) ∷ m a → (a → m b) → m b

(≫) ∷ (Monad m) ⇒ m a → m b → m b
xM ≫ yM = xM ≫= \ _ → yM

-- RebindableSyntax
(>>=) ∷ (Monad m) ⇒ m a → (a → m b) → m b
(>>=) = (≫=)

-- Rebindable Syntax
(>>) ∷ (Monad m) ⇒ m a → m b → m b
(>>) = (≫)

skip ∷ (Monad m) ⇒ m ()
skip = return ()

each𝐿 ∷ (Monad m) ⇒ (a → m ()) → 𝐿 a → m ()
each𝐿 f = fold𝐿 skip $ \ x yM → yM ≫ f x

eachWith𝐿 ∷ (Monad m) ⇒ 𝐿 a → (a → m ()) → m () 
eachWith𝐿 xs f = each𝐿 f xs

exec𝐿 ∷ (Monad m) ⇒ 𝐿 (m ()) → m () 
exec𝐿 = each𝐿 id

-- Monad Instances

instance Monad 𝐿 where
  return ∷ ∀ a. a → 𝐿 a
  return x = (x :& Nil)

  (≫=) ∷ ∀ a b. 𝐿 a → (a → 𝐿 b) → 𝐿 b
  Nil ≫= _ = Nil
  (x :& xs) ≫= f = f x ⧺ (xs ≫= f)

cartWith ∷ (a → b → c) → 𝐿 a → 𝐿 b → 𝐿 c
cartWith f xs ys = do
  x ← xs
  y ← ys
  return $ f x y

(⨳) ∷ 𝐿 a → 𝐿 b → 𝐿 (a ∧ b)
(⨳) = cartWith (\ x y → x :* y)

instance Monad 𝑂 where
  return ∷ ∀ a. a → 𝑂 a
  return x = Some x

  (≫=) ∷ ∀ a b. 𝑂 a → (a → 𝑂 b) → 𝑂 b
  None ≫= _ = None
  (Some x) ≫= f = f x

-- IO

instance Monad IO where
  return = HS.return
  (≫=) = (HS.>>=)

print ∷ 𝕊 → IO ()
print = Text.putStr

println ∷ 𝕊 → IO ()
println s = exec𝐿 $ list [print s,print "\n"]

printsln ∷ 𝐿 𝕊 → IO ()
printsln ss = exec𝐿 $ list [eachWith𝐿 ss print,print "\n"]

printlns ∷ 𝐿 𝕊 → IO ()
printlns ss = eachWith𝐿 ss println

abortIO ∷ IO a
abortIO = HS.exitWith $ HS.ExitFailure $ HS.fromIntegral 1

readInput ∷ IO 𝕊
readInput = Text.getContents

readFile ∷ 𝕊 → IO 𝕊
readFile = Text.readFile ∘ Text.unpack

writeFile ∷ 𝕊 → 𝕊 → IO ()
writeFile fn = Text.writeFile (Text.unpack fn)

trace ∷ 𝕊 → a → a
trace s x = HS.unsafePerformIO $ do
  println s
  return x

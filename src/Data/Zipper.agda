module src.Data.Zipper where

open import Agda.Builtin.Equality
open import Data.Nat
open import Data.Nat.Properties
open import Induction.WellFounded
open import Haskell.Prelude
open import Relation.Binary.PropositionalEquality

-- language extensions
{-# FOREIGN AGDA2HS
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
#-}

-- imports
{-# FOREIGN AGDA2HS
import Control.Arrow (first)
import Control.Comonad
import Control.Lens hiding ((<.>))
import Control.Monad
import Data.Foldable
import Data.Functor.Apply
import Data.Functor.Extend
import Data.Function
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup.Foldable
import GHC.Generics
#-}

record Zipper (a : Set) : Set where
  constructor MkZipper
  field
    prefix : List a
    focus  : a
    suffix : List a
{-# COMPILE AGDA2HS Zipper #-}

open Zipper public

{-# FOREIGN AGDA2HS
deriving instance Show a => Show (Zipper a)
deriving instance Eq a => Eq (Zipper a)
deriving instance Generic a => Generic (Zipper a)
deriving instance Functor Zipper
deriving instance Foldable Zipper
deriving instance Traversable Zipper

makeLenses ''Zipper
#-}

{-# TERMINATING #-}
repeat : a → List a
repeat x = x ∷ repeat x

instance
  isFunctorZipper : Functor Zipper
  isFunctorZipper .fmap k
    record { prefix = p ; focus = f ; suffix = s } =
    record { prefix = map k p ; focus = k f ; suffix = map k s }

  isApplicativeZipper : Applicative Zipper
  isApplicativeZipper .pure a =
    record { prefix = repeat a ; focus = a ; suffix = repeat a }
  isApplicativeZipper ._<*>_
    record { prefix = p1 ; focus = f1 ; suffix = s1 }
    record { prefix = p2 ; focus = f2 ; suffix = s2 } =
      record { prefix = zipWith id p1 p2 ; focus = f1 f2 ; suffix = zipWith id s1 s2 }
  {-# COMPILE AGDA2HS isApplicativeZipper #-}

  isFoldableZipper : Foldable Zipper
  isFoldableZipper .foldMap k
    record { prefix = p ; focus = f ; suffix = s } =
      foldMap k p <> k f <> foldMap k s

  isTraversableZipper : Traversable Zipper
  isTraversableZipper .traverse k
    record { prefix = p ; focus = f ; suffix = s } =
    ⦇ MkZipper (traverse k p) (k f) (traverse k s) ⦈

  isSemigroupZipper : ∀ {a : Set} → ⦃ Semigroup a ⦄ → Semigroup (Zipper a)
  isSemigroupZipper ._<>_
    record { prefix = p1 ; focus = f1 ; suffix = s1 }
    record { prefix = p2 ; focus = f2 ; suffix = s2 } =
    record { prefix = zipWith (_<>_) p1 p2
           ; focus = f1 <> f2
           ; suffix = zipWith (_<>_) s1 s2 }
  {-# COMPILE AGDA2HS isSemigroupZipper #-}

{-# FOREIGN AGDA2HS
-- | Many of these instances are from Tony Morris's package, list-zipper
instance Apply Zipper where
  MkZipper l1 x1 r1 <.> MkZipper l2 x2 r2 =
    MkZipper (zipWith id l1 l2) (x1 x2) (zipWith id r1 r2)

instance Foldable1 Zipper where
  foldMap1 f (MkZipper [] x []) =
    f x
  foldMap1 f (MkZipper [] x (rh : rt)) =
    f x <> foldMap1 f (rh :| rt)
  foldMap1 f (MkZipper (lh : lt) x []) =
    foldMap1 f (lh :| lt) <> f x
  foldMap1 f (MkZipper (lh : lt) x (rh : rt)) =
    foldMap1 f (lh :| lt) <> f x <> foldMap1 f (rh :| rt)

instance Traversable1 Zipper where
  traverse1 f (MkZipper [] x []) =
    (\x' -> MkZipper [] x' []) <$> f x
  traverse1 f (MkZipper (lh : lt) x []) =
    (\l' x' -> MkZipper (toList l') x' []) <$> traverse1 f (lh :| lt) <.> f x
  traverse1 f (MkZipper [] x (rh : rt)) =
    (\x' r' -> MkZipper [] x' (toList r')) <$> f x <.> traverse1 f (rh :| rt)
  traverse1 f (MkZipper (lh : lt) x (rh : rt)) =
    (\l' x' r' -> MkZipper (toList l') x' (toList r'))
      <$> traverse1 f (lh :| lt) <.> f x <.> traverse1 f (rh :| rt)

instance Extend Zipper where
  duplicated z =
    let dup x = (x, x)
        unf f = unfoldr (fmap dup . f) z
     in MkZipper (unf left) z (unf right)

instance Comonad Zipper where
  duplicate = duplicated
  extract (MkZipper _ x _) = x
#-}

left : ∀ {a : Set} → Zipper a → Maybe (Zipper a)
left record { prefix = [] ; focus = f ; suffix = s } = Nothing
left record { prefix = x ∷ p ; focus = f ; suffix = s } =
  Just record { prefix = p ; focus = x ; suffix = f ∷ s }
{-# COMPILE AGDA2HS left #-}

right : ∀ {a : Set} → Zipper a → Maybe (Zipper a)
right record { prefix = p ; focus = f ; suffix = [] } = Nothing
right record { prefix = p ; focus = f ; suffix = x ∷ s } =
  Just record { prefix = f ∷ p ; focus = x ; suffix = s }
{-# COMPILE AGDA2HS right #-}

left-right : ∀ {a : Set} (z r : Zipper a) → right z ≡ Just r → left r ≡ Just z
left-right record { prefix = p ; focus = f ; suffix = (x ∷ s) }
         .(record { prefix = f ∷ p ; focus = x ; suffix = s }) refl = refl

right-left : ∀ {a : Set} (z r : Zipper a) → left z ≡ Just r → right r ≡ Just z
right-left record { prefix = (x ∷ p) ; focus = f ; suffix = s }
         .(record { prefix = p ; focus = x ; suffix = f ∷ s }) refl = refl

fromList : List a → Maybe (Zipper a)
fromList [] = Nothing
fromList (x ∷ xs) = Just record { prefix = [] ; focus = x ; suffix = xs }
{-# COMPILE AGDA2HS fromList #-}

unzipper : Zipper a → List a
unzipper record { prefix = p ; focus = f ; suffix = s } = reverse p ++ f ∷ s
{-# COMPILE AGDA2HS unzipper #-}

overlay : Zipper a → List a → Maybe (Zipper a)
overlay record { prefix = p ; focus = _ ; suffix = [] } [] = Nothing
overlay record { prefix = xs ; focus = _ ; suffix = (z ∷ zs) } [] =
  Just record { prefix = xs ; focus = z ; suffix = zs }
overlay record { prefix = xs ; focus = _ ; suffix = zs } (w ∷ ws) =
  Just record { prefix = xs ; focus = w ; suffix = ws ++ zs }
{-# COMPILE AGDA2HS overlay #-}

record MonadPlus (m : Set → Set) : Set₁ where
  field
    mzero : m a
    mplus : m a → m a → m a
    overlap ⦃ super ⦄ : Monad m

open MonadPlus ⦃ ... ⦄ public

{-# COMPILE AGDA2HS MonadPlus existing-class #-}

instance
  isListMonadPlus : MonadPlus List
  isListMonadPlus = record { mzero = [] ; mplus = _++_ }

zipper : ⦃ MonadPlus f ⦄ → (a → Bool) → List a → f (Zipper a)
zipper f xs = case break f xs of λ where
  (ys , z ∷ zs) →
    return record { prefix = reverse ys ; focus = z ; suffix = zs }
  _ → mzero
{-# COMPILE AGDA2HS zipper #-}

spanM : ⦃ Monad m ⦄ → ⦃ MonadPlus f ⦄ → (a → m Bool) → List a → m (f a × List a)
spanM _ [] = return (mzero , [])
spanM {m} {f} {a} {{M}} p (x ∷ xs) = do
  true <- p x
    where
      false → return (mzero , x ∷ xs)
  (ys , zs) <- spanM {m} {f} {a} {{M}} p xs
  return (mplus (return x) ys , zs)
{-# COMPILE AGDA2HS spanM #-}

infixr 1 _>=>_
_>=>_ : ⦃ Monad m ⦄ → (a → m b) → (b → m c) → a → m c
f >=> g = λ x → f x >>= g

infixr 1 _<=<_
_<=<_ : ⦃ Monad m ⦄ → (b → m c) → (a → m b) → a → m c
f <=< g = g >=> f

breakM : ⦃ Monad m ⦄ → ⦃ MonadPlus f ⦄ → (a → m Bool) → List a → m (f a × List a)
breakM p = spanM $ return ∘ not <=< p
{-# COMPILE AGDA2HS breakM #-}

zipperM
  : {a : Set}
  → {m : Set → Set}
  → {f : Set → Set}
  → ⦃ Monad m ⦄
  → ⦃ MonadPlus f ⦄
  → (a → m Bool)
  → List a
  → m (f (Zipper a))
zipperM {a} {_} {f} {{M}} k xs =
  breakM {{M}} {{isListMonadPlus}} k xs <&> λ where
    (ys , z ∷ zs) →
      return record { prefix = reverse ys ; focus = z ; suffix = zs }
    _ → mzero
{-# COMPILE AGDA2HS zipperM #-}

Traversal' : Set → Set → Set₁
Traversal' s a =
  ∀ {f : Set → Set} → ⦃ Applicative f ⦄ → (a → f a) → (s → f s)

items : Traversal' (Zipper a) a
items k z =
  ⦇ MkZipper (reverse <$> traverse k (reverse (prefix z)))
             (k (focus z))
             (traverse k (suffix z)) ⦈
{-# COMPILE AGDA2HS items #-}

scanPreState : {s : Set} → (a → s → (b × s)) → s → List a → List (b × s)
scanPreState f _ [] = []
scanPreState f s (x ∷ xs) =
  case f x s of λ where
    (b , s') → (b , s) ∷ scanPreState f s' xs
{-# COMPILE AGDA2HS scanPreState #-}

forward : {A : Set} → Zipper A → List A
forward z = focus z ∷ suffix z

measure : {A : Set}(f : A → ℕ) → Zipper A → ℕ
measure f z = sum (map (suc ∘ f) (forward z))

just-inj : {A : Set} {x y : A}
  → Just x ≡ Just y
  → x ≡ y
just-inj refl = refl

suffix-forward : {A : Set} {z z' : Zipper A}
  → right z ≡ Just z'
  → suffix z ≡ forward z'
suffix-forward {z = MkZipper prefix₁ focus₁ (x ∷ suffix₁)} H
  rewrite sym (just-inj H) = refl

measure-right-< : {A : Set} (m : A → ℕ) {z z' : Zipper A}
  → right z ≡ Just z'
  → measure m z' Data.Nat.< measure m z
measure-right-< m {z} {z'} H
  rewrite suffix-forward H = s≤s (m≤n+m _ _)

record ZipperMono (A : Set)(m : A → ℕ) : Set where
  constructor zippermono
  field
    func : Zipper A → Zipper A
    mono : ∀ z → measure m z ≡ measure m (func z)

open import Induction.WellFounded
open import Data.Nat.Induction

-- This version of survey shows that it terminates, but only if the function
-- passed is monotonic with respect to a measure on its input and output.
survey′ : {a : Set} (m : a → ℕ) → ZipperMono a m -> List a -> List a
survey′ {a} m (zippermono f mono) =
  maybe [] (λ z → go z (<-wellFounded _)) ∘ fromList
  where
    go : (z : Zipper a) → Acc Data.Nat._<_ (measure m z) → List a
    go z (acc rs) with f z | inspect f z
    ...| z' | [ eq ] with right z' | inspect right z'
    ...| Nothing | _ = unzipper z'
    ...| Just z''  | [ eq' ] =
      go z'' (rs (measure m z'')
         (≤-trans (measure-right-< m eq')
                  (≤-reflexive (trans (cong (measure m) (sym eq))
                               (sym (mono z))))))

{-# TERMINATING #-}
survey : {a : Set} → (Zipper a -> Zipper a) -> List a -> List a
survey {a} f = maybe [] go ∘ fromList
  where
    go : Zipper a → List a
    go z = let z' = f z in maybe (unzipper z') go (right z')
{-# COMPILE AGDA2HS survey #-}

{-# TERMINATING #-}
surveyM
  : {a : Set}
  → {m : Set → Set}
  → ⦃ Monad m ⦄
  → (Zipper a → m (Zipper a))
  → List a
  → m (List a)
surveyM {a} {m} f = maybe (return []) go ∘ fromList
  where
    go : Zipper a → m (List a)
    go z = do
      z' <- f z
      maybe (return (unzipper z')) go (right z')
{-# COMPILE AGDA2HS surveyM #-}

first : (a → b) → (a × c) → (b × c)
first f (a , c) = (f a , c)

mapUntils
  : (List a → List a)
  → (a → Maybe (List a × b))
  → List a
  → Maybe (List a × b)
mapUntils rev k [] = Nothing
mapUntils rev k (x ∷ xs) = case k x of λ where
  (Just (xs' , b)) → Just (rev xs' ++ xs , b)
  Nothing → first (λ xs → x ∷ xs) <$> mapUntils rev k xs
{-# COMPILE AGDA2HS mapUntils #-}

-- | Given a zipper list, attempt to locate an element first in the prefix,
--   then in the suffix, and allow for a transformation of that sub-zipper
--   list within the parent list, plus the generation of some datum.
mapLeftThenRightUntils :
  Zipper a → (Bool → a → Maybe (List a × b)) → Maybe (Zipper a × b)
mapLeftThenRightUntils {a} {b} z f =
  case mapUntils reverse (f true) (prefix z) of λ where
    (Just (p' , b)) → Just (record z { prefix = p' } , b)
    Nothing →
      case mapUntils id (f false) (suffix z) of λ where
        (Just (s' , b)) → Just (record z { suffix = s' } , b)
        Nothing → Nothing
{-# COMPILE AGDA2HS mapLeftThenRightUntils #-}

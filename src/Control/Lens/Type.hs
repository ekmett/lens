{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE Trustworthy #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Type
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- This module exports the majority of the types that need to appear in user
-- signatures or in documentation when talking about lenses. The remaining types
-- for consuming lenses are distributed across various modules in the hierarchy.
-------------------------------------------------------------------------------
module Control.Lens.Type
  (
  -- * Other
    Equality, Equality', As
  , Iso, Iso'
  , Prism , Prism'
  , Review , AReview
  -- * Lenses, Folds and Traversals
  , Lens, Lens'
  , Traversal, Traversal'
  , Traversal1, Traversal1'
  , Setter, Setter'
  , Getter, Fold
  , Fold1
  -- * Indexed
  , IndexedLens, IndexedLens'
  , IndexedTraversal, IndexedTraversal'
  , IndexedTraversal1, IndexedTraversal1'
  , IndexedSetter, IndexedSetter'
  , IndexedGetter, IndexedFold
  , IndexedFold1
  -- * Index-Preserving
  , IndexPreservingLens, IndexPreservingLens'
  , IndexPreservingTraversal, IndexPreservingTraversal'
  , IndexPreservingTraversal1, IndexPreservingTraversal1'
  , IndexPreservingSetter, IndexPreservingSetter'
  , IndexPreservingGetter, IndexPreservingFold
  , IndexPreservingFold1
  -- * Common
  , Simple
  , LensLike, LensLike'
  , Over, Over'
  , IndexedLensLike, IndexedLensLike'
  , Optical, Optical'
  , Optic, Optic'
  ) where

import Prelude ()

import Control.Lens.Internal.Prelude
import Control.Lens.Internal.Setter
import Control.Lens.Internal.Indexed
import Data.Bifunctor
import Data.Functor.Apply
import Data.Kind

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g,h)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g
-- >>> let h :: Expr -> Expr -> Expr; h = Debug.SimpleReflect.Vars.h
-- >>> let getter :: Expr -> Expr; getter = fun "getter"
-- >>> let setter :: Expr -> Expr -> Expr; setter = fun "setter"
-- >>> import Numeric.Natural
-- >>> let nat :: Prism' Integer Natural; nat = prism toInteger $ \i -> if i < 0 then Left i else Right (fromInteger i)

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

-- | A 'Lens' is actually a lens family as described in
-- <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- With great power comes great responsibility and a 'Lens' is subject to the
-- three common sense 'Lens' laws:
--
-- 1) You get back what you put in:
--
-- @
-- 'Control.Lens.Getter.view' l ('Control.Lens.Setter.set' l v s)  ≡ v
-- @
--
-- 2) Putting back what you got doesn't change anything:
--
-- @
-- 'Control.Lens.Setter.set' l ('Control.Lens.Getter.view' l s) s  ≡ s
-- @
--
-- 3) Setting twice is the same as setting once:
--
-- @
-- 'Control.Lens.Setter.set' l v' ('Control.Lens.Setter.set' l v s) ≡ 'Control.Lens.Setter.set' l v' s
-- @
--
-- These laws are strong enough that the 4 type parameters of a 'Lens' cannot
-- vary fully independently. For more on how they interact, read the \"Why is
-- it a Lens Family?\" section of
-- <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- There are some emergent properties of these laws:
--
-- 1) @'Control.Lens.Setter.set' l s@ must be injective for every @s@ This is a consequence of law #1
--
-- 2) @'Control.Lens.Setter.set' l@ must be surjective, because of law #2, which indicates that it is possible to obtain any 'v' from some 's' such that @'Control.Lens.Setter.set' s v = s@
--
-- 3) Given just the first two laws you can prove a weaker form of law #3 where the values @v@ that you are setting match:
--
-- @
-- 'Control.Lens.Setter.set' l v ('Control.Lens.Setter.set' l v s) ≡ 'Control.Lens.Setter.set' l v s
-- @
--
-- Every 'Lens' can be used directly as a 'Control.Lens.Setter.Setter' or 'Traversal'.
--
-- You can also use a 'Lens' for 'Control.Lens.Getter.Getting' as if it were a
-- 'Fold' or 'Getter'.
--
-- Since every 'Lens' is a valid 'Traversal', the
-- 'Traversal' laws are required of any 'Lens' you create:
--
-- @
-- l 'pure' ≡ 'pure'
-- 'fmap' (l f) '.' l g ≡ 'Data.Functor.Compose.getCompose' '.' l ('Data.Functor.Compose.Compose' '.' 'fmap' f '.' g)
-- @
--
-- @
-- type 'Lens' s t a b = forall f. 'Functor' f => 'LensLike' f s t a b
-- @
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | @
-- type 'Lens'' = 'Simple' 'Lens'
-- @
type Lens' s a = Lens s s a a

-- | Every 'IndexedLens' is a valid 'Lens' and a valid 'Control.Lens.Traversal.IndexedTraversal'.
type IndexedLens i s t a b = forall f p. (Indexable i p, Functor f) => p a (f b) -> s -> f t

-- | @
-- type 'IndexedLens'' i = 'Simple' ('IndexedLens' i)
-- @
type IndexedLens' i s a = IndexedLens i s s a a

-- | An 'IndexPreservingLens' leaves any index it is composed with alone.
type IndexPreservingLens s t a b = forall p f. (Conjoined p, Functor f) => p a (f b) -> p s (f t)

-- | @
-- type 'IndexPreservingLens'' = 'Simple' 'IndexPreservingLens'
-- @
type IndexPreservingLens' s a = IndexPreservingLens s s a a

------------------------------------------------------------------------------
-- Traversals
------------------------------------------------------------------------------

-- | A 'Traversal' can be used directly as a 'Control.Lens.Setter.Setter' or a 'Fold' (but not as a 'Lens') and provides
-- the ability to both read and update multiple fields, subject to some relatively weak 'Traversal' laws.
--
-- These have also been known as multilenses, but they have the signature and spirit of
--
-- @
-- 'Data.Traversable.traverse' :: 'Data.Traversable.Traversable' f => 'Traversal' (f a) (f b) a b
-- @
--
-- and the more evocative name suggests their application.
--
-- Most of the time the 'Traversal' you will want to use is just 'Data.Traversable.traverse', but you can also pass any
-- 'Lens' or 'Iso' as a 'Traversal', and composition of a 'Traversal' (or 'Lens' or 'Iso') with a 'Traversal' (or 'Lens' or 'Iso')
-- using ('.') forms a valid 'Traversal'.
--
-- The laws for a 'Traversal' @t@ follow from the laws for 'Data.Traversable.Traversable' as stated in \"The Essence of the Iterator Pattern\".
--
-- @
-- t 'pure' ≡ 'pure'
-- 'fmap' (t f) '.' t g ≡ 'Data.Functor.Compose.getCompose' '.' t ('Data.Functor.Compose.Compose' '.' 'fmap' f '.' g)
-- @
--
-- One consequence of this requirement is that a 'Traversal' needs to leave the same number of elements as a
-- candidate for subsequent 'Traversal' that it started with. Another testament to the strength of these laws
-- is that the caveat expressed in section 5.5 of the \"Essence of the Iterator Pattern\" about exotic
-- 'Data.Traversable.Traversable' instances that 'Data.Traversable.traverse' the same entry multiple times was actually already ruled out by the
-- second law in that same paper!
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- | @
-- type 'Traversal'' = 'Simple' 'Traversal'
-- @
type Traversal' s a = Traversal s s a a

type Traversal1 s t a b = forall f. Apply f => (a -> f b) -> s -> f t
type Traversal1' s a = Traversal1 s s a a

-- | Every 'IndexedTraversal' is a valid 'Control.Lens.Traversal.Traversal' or
-- 'Control.Lens.Fold.IndexedFold'.
--
-- The 'Indexed' constraint is used to allow an 'IndexedTraversal' to be used
-- directly as a 'Control.Lens.Traversal.Traversal'.
--
-- The 'Control.Lens.Traversal.Traversal' laws are still required to hold.
--
-- In addition, the index @i@ should satisfy the requirement that it stays
-- unchanged even when modifying the value @a@, otherwise traversals like
-- 'indices' break the 'Traversal' laws.
type IndexedTraversal i s t a b = forall p f. (Indexable i p, Applicative f) => p a (f b) -> s -> f t

-- | @
-- type 'IndexedTraversal'' i = 'Simple' ('IndexedTraversal' i)
-- @
type IndexedTraversal' i s a = IndexedTraversal i s s a a

type IndexedTraversal1 i s t a b = forall p f. (Indexable i p, Apply f) => p a (f b) -> s -> f t
type IndexedTraversal1' i s a = IndexedTraversal1 i s s a a

-- | An 'IndexPreservingLens' leaves any index it is composed with alone.
type IndexPreservingTraversal s t a b = forall p f. (Conjoined p, Applicative f) => p a (f b) -> p s (f t)

-- | @
-- type 'IndexPreservingTraversal'' = 'Simple' 'IndexPreservingTraversal'
-- @
type IndexPreservingTraversal' s a = IndexPreservingTraversal s s a a

type IndexPreservingTraversal1 s t a b = forall p f. (Conjoined p, Apply f) => p a (f b) -> p s (f t)
type IndexPreservingTraversal1' s a = IndexPreservingTraversal1 s s a a

------------------------------------------------------------------------------
-- Setters
------------------------------------------------------------------------------

-- | The only 'LensLike' law that can apply to a 'Setter' @l@ is that
--
-- @
-- 'Control.Lens.Setter.set' l y ('Control.Lens.Setter.set' l x a) ≡ 'Control.Lens.Setter.set' l y a
-- @
--
-- You can't 'Control.Lens.Getter.view' a 'Setter' in general, so the other two laws are irrelevant.
--
-- However, two 'Functor' laws apply to a 'Setter':
--
-- @
-- 'Control.Lens.Setter.over' l 'id' ≡ 'id'
-- 'Control.Lens.Setter.over' l f '.' 'Control.Lens.Setter.over' l g ≡ 'Control.Lens.Setter.over' l (f '.' g)
-- @
--
-- These can be stated more directly:
--
-- @
-- l 'pure' ≡ 'pure'
-- l f '.' 'untainted' '.' l g ≡ l (f '.' 'untainted' '.' g)
-- @
--
-- You can compose a 'Setter' with a 'Lens' or a 'Traversal' using ('.') from the @Prelude@
-- and the result is always only a 'Setter' and nothing more.
--
-- >>> over traverse f [a,b,c,d]
-- [f a,f b,f c,f d]
--
-- >>> over _1 f (a,b)
-- (f a,b)
--
-- >>> over (traverse._1) f [(a,b),(c,d)]
-- [(f a,b),(f c,d)]
--
-- >>> over both f (a,b)
-- (f a,f b)
--
-- >>> over (traverse.both) f [(a,b),(c,d)]
-- [(f a,f b),(f c,f d)]
type Setter s t a b = forall f. Settable f => (a -> f b) -> s -> f t

-- | A 'Setter'' is just a 'Setter' that doesn't change the types.
--
-- These are particularly common when talking about monomorphic containers. /e.g./
--
-- @
-- 'sets' Data.Text.map :: 'Setter'' 'Data.Text.Internal.Text' 'Char'
-- @
--
-- @
-- type 'Setter'' = 'Simple' 'Setter'
-- @
type Setter' s a = Setter s s a a

-- | Every 'IndexedSetter' is a valid 'Setter'.
--
-- The 'Setter' laws are still required to hold.
type IndexedSetter i s t a b = forall f p.
  (Indexable i p, Settable f) => p a (f b) -> s -> f t

-- | @
-- type 'IndexedSetter'' i = 'Simple' ('IndexedSetter' i)
-- @
type IndexedSetter' i s a = IndexedSetter i s s a a

-- | An 'IndexPreservingSetter' can be composed with a 'IndexedSetter', 'IndexedTraversal' or 'IndexedLens'
-- and leaves the index intact, yielding an 'IndexedSetter'.
type IndexPreservingSetter s t a b = forall p f. (Conjoined p, Settable f) => p a (f b) -> p s (f t)

-- | @
-- type 'IndexedPreservingSetter'' i = 'Simple' 'IndexedPreservingSetter'
-- @
type IndexPreservingSetter' s a = IndexPreservingSetter s s a a

-----------------------------------------------------------------------------
-- Isomorphisms
-----------------------------------------------------------------------------

-- | Isomorphism families can be composed with another 'Lens' using ('.') and 'id'.
--
-- Since every 'Iso' is both a valid 'Lens' and a valid 'Prism', the laws for those types
-- imply the following laws for an 'Iso' 'f':
--
-- @
-- f '.' 'Control.Lens.Iso.from' f ≡ 'id'
-- 'Control.Lens.Iso.from' f '.' f ≡ 'id'
-- @
--
-- Note: Composition with an 'Iso' is index- and measure- preserving.
type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

-- | @
-- type 'Iso'' = 'Control.Lens.Type.Simple' 'Iso'
-- @
type Iso' s a = Iso s s a a

------------------------------------------------------------------------------
-- Review Internals
------------------------------------------------------------------------------

-- | This is a limited form of a 'Prism' that can only be used for 're' operations.
--
-- Like with a 'Getter', there are no laws to state for a 'Review'.
--
-- You can generate a 'Review' by using 'unto'. You can also use any 'Prism' or 'Iso'
-- directly as a 'Review'.
type Review t b = forall p f. (Choice p, Bifunctor p, Settable f) => Optic' p f t b

-- | If you see this in a signature for a function, the function is expecting a 'Review'
-- (in practice, this usually means a 'Prism').
type AReview t b = Optic' Tagged Identity t b

------------------------------------------------------------------------------
-- Prism Internals
------------------------------------------------------------------------------

-- | A 'Prism' @l@ is a 'Traversal' that can also be turned
-- around with 'Control.Lens.Review.re' to obtain a 'Getter' in the
-- opposite direction.
--
-- There are three laws that a 'Prism' should satisfy:
--
-- First, if I 'Control.Lens.Review.re' or 'Control.Lens.Review.review' a value with a 'Prism' and then 'Control.Lens.Fold.preview' or use ('Control.Lens.Fold.^?'), I will get it back:
--
-- @
-- 'Control.Lens.Fold.preview' l ('Control.Lens.Review.review' l b) ≡ 'Just' b
-- @
--
-- Second, if you can extract a value @a@ using a 'Prism' @l@ from a value @s@, then the value @s@ is completely described by @l@ and @a@:
--
-- @
-- 'Control.Lens.Fold.preview' l s ≡ 'Just' a ⟹ 'Control.Lens.Review.review' l a ≡ s
-- @
--
-- Third, if you get non-match @t@, you can convert it result back to @s@:
--
-- @
-- 'Control.Lens.Combinators.matching' l s ≡ 'Left' t ⟹ 'Control.Lens.Combinators.matching' l t ≡ 'Left' s
-- @
--
-- The first two laws imply that the 'Traversal' laws hold for every 'Prism' and that we 'Data.Traversable.traverse' at most 1 element:
--
-- @
-- 'Control.Lens.Fold.lengthOf' l x '<=' 1
-- @
--
-- It may help to think of this as an 'Iso' that can be partial in one direction.
--
-- Every 'Prism' is a valid 'Traversal'.
--
-- Every 'Iso' is a valid 'Prism'.
--
-- For example, you might have a @'Prism'' 'Integer' 'Numeric.Natural.Natural'@ allows you to always
-- go from a 'Numeric.Natural.Natural' to an 'Integer', and provide you with tools to check if an 'Integer' is
-- a 'Numeric.Natural.Natural' and/or to edit one if it is.
--
--
-- @
-- 'nat' :: 'Prism'' 'Integer' 'Numeric.Natural.Natural'
-- 'nat' = 'Control.Lens.Prism.prism' 'toInteger' '$' \\ i ->
--    if i '<' 0
--    then 'Left' i
--    else 'Right' ('fromInteger' i)
-- @
--
-- Now we can ask if an 'Integer' is a 'Numeric.Natural.Natural'.
--
-- >>> 5^?nat
-- Just 5
--
-- >>> (-5)^?nat
-- Nothing
--
-- We can update the ones that are:
--
-- >>> (-3,4) & both.nat *~ 2
-- (-3,8)
--
-- And we can then convert from a 'Numeric.Natural.Natural' to an 'Integer'.
--
-- >>> 5 ^. re nat -- :: Natural
-- 5
--
-- Similarly we can use a 'Prism' to 'Data.Traversable.traverse' the 'Left' half of an 'Either':
--
-- >>> Left "hello" & _Left %~ length
-- Left 5
--
-- or to construct an 'Either':
--
-- >>> 5^.re _Left
-- Left 5
--
-- such that if you query it with the 'Prism', you will get your original input back.
--
-- >>> 5^.re _Left ^? _Left
-- Just 5
--
-- Another interesting way to think of a 'Prism' is as the categorical dual of a 'Lens'
-- -- a co-'Lens', so to speak. This is what permits the construction of 'Control.Lens.Prism.outside'.
--
-- Note: Composition with a 'Prism' is index-preserving.
type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

-- | A 'Simple' 'Prism'.
type Prism' s a = Prism s s a a

-------------------------------------------------------------------------------
-- Equality
-------------------------------------------------------------------------------

-- | A witness that @(a ~ s, b ~ t)@.
--
-- Note: Composition with an 'Equality' is index-preserving.
type Equality (s :: k1) (t :: k2) (a :: k1) (b :: k2) = forall k3 (p :: k1 -> k3 -> Type) (f :: k2 -> k3) .
    p a (f b) -> p s (f t)

-- | A 'Simple' 'Equality'.
type Equality' s a = Equality s s a a

-- | Composable `asTypeOf`. Useful for constraining excess
-- polymorphism, @foo . (id :: As Int) . bar@.
type As a = Equality' a a

-------------------------------------------------------------------------------
-- Getters
-------------------------------------------------------------------------------

-- | A 'Getter' describes how to retrieve a single value in a way that can be
-- composed with other 'LensLike' constructions.
--
-- Unlike a 'Lens' a 'Getter' is read-only. Since a 'Getter'
-- cannot be used to write back there are no 'Lens' laws that can be applied to
-- it. In fact, it is isomorphic to an arbitrary function from @(s -> a)@.
--
-- Moreover, a 'Getter' can be used directly as a 'Control.Lens.Fold.Fold',
-- since it just ignores the 'Applicative'.
type Getter s a = forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s

-- | Every 'IndexedGetter' is a valid 'Control.Lens.Fold.IndexedFold' and can be used for 'Control.Lens.Getter.Getting' like a 'Getter'.
type IndexedGetter i s a = forall p f. (Indexable i p, Contravariant f, Functor f) => p a (f a) -> s -> f s

-- | An 'IndexPreservingGetter' can be used as a 'Getter', but when composed with an 'IndexedTraversal',
-- 'IndexedFold', or 'IndexedLens' yields an 'IndexedFold', 'IndexedFold' or 'IndexedGetter' respectively.
type IndexPreservingGetter s a = forall p f. (Conjoined p, Contravariant f, Functor f) => p a (f a) -> p s (f s)

--------------------------
-- Folds
--------------------------

-- | A 'Fold' describes how to retrieve multiple values in a way that can be composed
-- with other 'LensLike' constructions.
--
-- A @'Fold' s a@ provides a structure with operations very similar to those of the 'Data.Foldable.Foldable'
-- typeclass, see 'Control.Lens.Fold.foldMapOf' and the other 'Fold' combinators.
--
-- By convention, if there exists a 'foo' method that expects a @'Data.Foldable.Foldable' (f a)@, then there should be a
-- @fooOf@ method that takes a @'Fold' s a@ and a value of type @s@.
--
-- A 'Getter' is a legal 'Fold' that just ignores the supplied 'Data.Monoid.Monoid'.
--
-- Unlike a 'Control.Lens.Traversal.Traversal' a 'Fold' is read-only. Since a 'Fold' cannot be used to write back
-- there are no 'Lens' laws that apply.
type Fold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

-- | Every 'IndexedFold' is a valid 'Control.Lens.Fold.Fold' and can be used for 'Control.Lens.Getter.Getting'.
type IndexedFold i s a = forall p f.  (Indexable i p, Contravariant f, Applicative f) => p a (f a) -> s -> f s

-- | An 'IndexPreservingFold' can be used as a 'Fold', but when composed with an 'IndexedTraversal',
-- 'IndexedFold', or 'IndexedLens' yields an 'IndexedFold' respectively.
type IndexPreservingFold s a = forall p f. (Conjoined p, Contravariant f, Applicative f) => p a (f a) -> p s (f s)

-- | A relevant Fold (aka 'Fold1') has one or more targets.
type Fold1 s a = forall f. (Contravariant f, Apply f) => (a -> f a) -> s -> f s
type IndexedFold1 i s a = forall p f.  (Indexable i p, Contravariant f, Apply f) => p a (f a) -> s -> f s
type IndexPreservingFold1 s a = forall p f. (Conjoined p, Contravariant f, Apply f) => p a (f a) -> p s (f s)

-------------------------------------------------------------------------------
-- Simple Overloading
-------------------------------------------------------------------------------

-- | A 'Simple' 'Lens', 'Simple' 'Traversal', ... can
-- be used instead of a 'Lens','Traversal', ...
-- whenever the type variables don't change upon setting a value.
--
-- @
-- 'Data.Complex.Lens._imagPart' :: 'Simple' 'Lens' ('Data.Complex.Complex' a) a
-- 'Control.Lens.Traversal.traversed' :: 'Simple' ('IndexedTraversal' 'Int') [a] a
-- @
--
-- Note: To use this alias in your own code with @'LensLike' f@ or
-- 'Setter', you may have to turn on @LiberalTypeSynonyms@.
--
-- This is commonly abbreviated as a \"prime\" marker, /e.g./ 'Lens'' = 'Simple' 'Lens'.
type Simple f s a = f s s a a

-------------------------------------------------------------------------------
-- Optics
-------------------------------------------------------------------------------

-- | A valid 'Optic' @l@ should satisfy the laws:
--
-- @
-- l 'pure' ≡ 'pure'
-- l ('Procompose' f g) = 'Procompose' (l f) (l g)
-- @
--
-- This gives rise to the laws for 'Equality', 'Iso', 'Prism', 'Lens',
-- 'Traversal', 'Traversal1', 'Setter', 'Fold', 'Fold1', and 'Getter' as well
-- along with their index-preserving variants.
--
-- @
-- type 'LensLike' f s t a b = 'Optic' (->) f s t a b
-- @
type Optic p f s t a b = p a (f b) -> p s (f t)

-- | @
-- type 'Optic'' p f s a = 'Simple' ('Optic' p f) s a
-- @
type Optic' p f s a = Optic p f s s a a

-- | @
-- type 'LensLike' f s t a b = 'Optical' (->) (->) f s t a b
-- @
--
-- @
-- type 'Over' p f s t a b = 'Optical' p (->) f s t a b
-- @
--
-- @
-- type 'Optic' p f s t a b = 'Optical' p p f s t a b
-- @
type Optical p q f s t a b = p a (f b) -> q s (f t)

-- | @
-- type 'Optical'' p q f s a = 'Simple' ('Optical' p q f) s a
-- @
type Optical' p q f s a = Optical p q f s s a a


-- | Many combinators that accept a 'Lens' can also accept a
-- 'Traversal' in limited situations.
--
-- They do so by specializing the type of 'Functor' that they require of the
-- caller.
--
-- If a function accepts a @'LensLike' f s t a b@ for some 'Functor' @f@,
-- then they may be passed a 'Lens'.
--
-- Further, if @f@ is an 'Applicative', they may also be passed a
-- 'Traversal'.
type LensLike f s t a b = (a -> f b) -> s -> f t

-- | @
-- type 'LensLike'' f = 'Simple' ('LensLike' f)
-- @
type LensLike' f s a = LensLike f s s a a

-- | Convenient alias for constructing indexed lenses and their ilk.
type IndexedLensLike i f s t a b = forall p. Indexable i p => p a (f b) -> s -> f t

-- | Convenient alias for constructing simple indexed lenses and their ilk.
type IndexedLensLike' i f s a = IndexedLensLike i f s s a a

-- | This is a convenient alias for use when you need to consume either indexed or non-indexed lens-likes based on context.
type Over p f s t a b = p a (f b) -> s -> f t

-- | This is a convenient alias for use when you need to consume either indexed or non-indexed lens-likes based on context.
--
-- @
-- type 'Over'' p f = 'Simple' ('Over' p f)
-- @
type Over' p f s a = Over p f s s a a

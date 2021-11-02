{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeInType #-}

#include "lens-common.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Iso
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types, TypeFamilies, FunctionalDependencies
--
----------------------------------------------------------------------------
module Control.Lens.Iso
  (
  -- * Isomorphism Lenses
    Iso, Iso'
  , AnIso, AnIso'
  -- * Isomorphism Construction
  , iso
  -- * Consuming Isomorphisms
  , from
  , cloneIso
  , withIso
  -- * Working with isomorphisms
  , au
  , auf
  , xplat
  , xplatf
  , under
  , mapping
  -- ** Common Isomorphisms
  , simple
  , non, non'
  , anon
  , enum
  , curried, uncurried
  , flipped
  , swapped
  , pattern Swapped
  , strict, lazy
  , pattern Strict
  , pattern Lazy
  , Reversing(..)
  , reversed
  , pattern Reversed
  , involuted
  , pattern List
  -- ** Uncommon Isomorphisms
  , magma
  , imagma
  , Magma
  -- ** Contravariant functors
  , contramapping
  -- * Profunctors
  , Profunctor(dimap,rmap,lmap)
  , dimapping
  , lmapping
  , rmapping
  -- * Bifunctors
  , bimapping
  , firsting
  , seconding
  -- * Coercions
  , coerced
  ) where

import Control.Lens.Equality (simple)
import Control.Lens.Getter
import Control.Lens.Fold
import Control.Lens.Internal.Context
import Control.Lens.Internal.Indexed
import Control.Lens.Internal.Iso as Iso
import Control.Lens.Internal.Magma
import Control.Lens.Prism
import Control.Lens.Review
import Control.Lens.Type

import Data.Bifunctor
import Data.Bifunctor.Swap (Swap (..))
import Data.Functor.Identity
import Data.Strict.Classes (Strict (..))
import Data.Maybe
import Data.Profunctor
import Data.Profunctor.Unsafe

import Data.Coerce

import qualified GHC.Exts as Exts
import GHC.Exts (TYPE)

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import qualified Data.Map as Map
-- >>> import Data.Foldable
-- >>> import Data.Monoid

----------------------------------------------------------------------------
-- Isomorphisms
-----------------------------------------------------------------------------

-- | When you see this as an argument to a function, it expects an 'Iso'.
type AnIso s t a b = Exchange a b a (Identity b) -> Exchange a b s (Identity t)

-- | A 'Simple' 'AnIso'.
type AnIso' s a = AnIso s s a a


-- | Build a simple isomorphism from a pair of inverse functions.
--
-- @
-- 'Control.Lens.Getter.view' ('iso' f g) ≡ f
-- 'Control.Lens.Getter.view' ('Control.Lens.Iso.from' ('iso' f g)) ≡ g
-- 'Control.Lens.Setter.over' ('iso' f g) h ≡ g '.' h '.' f
-- 'Control.Lens.Setter.over' ('Control.Lens.Iso.from' ('iso' f g)) h ≡ f '.' h '.' g
-- @
iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

----------------------------------------------------------------------------
-- Consuming Isomorphisms
-----------------------------------------------------------------------------

-- | Invert an isomorphism.
--
-- @
-- 'from' ('from' l) ≡ l
-- @
from :: AnIso s t a b -> Iso b a t s
from l = withIso l $ \sa bt -> iso bt sa
{-# INLINE from #-}

-- | Extract the two functions, one from @s -> a@ and
-- one from @b -> t@ that characterize an 'Iso'.
withIso :: forall s t a b rep (r :: TYPE rep).
             AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k = case ai (Exchange id Identity) of
  Exchange sa bt -> k sa (runIdentity #. bt)
{-# INLINE withIso #-}

-- | Convert from 'AnIso' back to any 'Iso'.
--
-- This is useful when you need to store an isomorphism as a data type inside a container
-- and later reconstitute it as an overloaded function.
--
-- See 'Control.Lens.Lens.cloneLens' or 'Control.Lens.Traversal.cloneTraversal' for more information on why you might want to do this.
cloneIso :: AnIso s t a b -> Iso s t a b
cloneIso k = withIso k $ \sa bt -> iso sa bt
{-# INLINE cloneIso #-}

-----------------------------------------------------------------------------
-- Isomorphisms families as Lenses
-----------------------------------------------------------------------------

-- | Based on 'Control.Lens.Wrapped.ala' from Conor McBride's work on Epigram.
--
-- This version is generalized to accept any 'Iso', not just a @newtype@.
--
-- >>> au (_Wrapping Sum) foldMap [1,2,3,4]
-- 10
--
-- You may want to think of this combinator as having the following, simpler type:
--
-- @
-- au :: AnIso s t a b -> ((b -> t) -> e -> s) -> e -> a
-- @
--
-- @
-- au = xplat . from
-- @
au :: Functor f => AnIso s t a b -> ((b -> t) -> f s) -> f a
au k = withIso k $ \ sa bt f -> fmap sa (f bt)
{-# INLINE au #-}

-- | Based on @ala'@ from Conor McBride's work on Epigram.
--
-- This version is generalized to accept any 'Iso', not just a @newtype@.
--
-- For a version you pass the name of the @newtype@ constructor to, see 'Control.Lens.Wrapped.alaf'.
--
-- >>> auf (_Wrapping Sum) (foldMapOf both) Prelude.length ("hello","world")
-- 10
--
-- Mnemonically, the German /auf/ plays a similar role to /à la/, and the combinator
-- is 'au' with an extra function argument:
--
-- @
-- 'auf' :: 'Iso' s t a b -> ((r -> t) -> e -> s) -> (r -> b) -> e -> a
-- @
--
-- but the signature is general.
--
-- Note: The direction of the 'Iso' required for this function changed in @lens@ 4.18 to match up
-- with the behavior of 'au'. For the old behavior use 'xplatf' or for a version that is compatible
-- across both old and new versions of @lens@ you can just use 'coerce'!
auf :: (Functor f, Functor g) => AnIso s t a b -> (f t -> g s) -> f b -> g a
auf k ftgs fb = withIso k $ \sa bt -> sa <$> ftgs (bt <$> fb)
{-# INLINE auf #-}

-- | @'xplat' = 'au' . 'from'@ but with a nicer signature.
xplat :: Optic (Costar ((->) s)) g s t a b -> ((s -> a) -> g b) -> g t
xplat f g = xplatf f g id

-- | @'xplatf' = 'auf' . 'from'@ but with a nicer signature.
--
-- >>> xplatf (_Unwrapping Sum) (foldMapOf both) Prelude.length ("hello","world")
-- 10
--
-- @
-- 'xplatf' :: 'Iso' s t a b -> ((r -> a) -> e -> b) -> (r -> s) -> e -> t
-- @
xplatf :: Optic (Costar f) g s t a b -> (f a -> g b) -> f s -> g t
xplatf = coerce
{-# INLINE xplat #-}

-- | The opposite of working 'Control.Lens.Setter.over' a 'Setter' is working 'under' an isomorphism.
--
-- @
-- 'under' ≡ 'Control.Lens.Setter.over' '.' 'from'
-- @
--
-- @
-- 'under' :: 'Iso' s t a b -> (t -> s) -> b -> a
-- @
under :: AnIso s t a b -> (t -> s) -> b -> a
under k = withIso k $ \ sa bt ts -> sa . ts . bt
{-# INLINE under #-}

-----------------------------------------------------------------------------
-- Isomorphisms
-----------------------------------------------------------------------------

-- | This isomorphism can be used to convert to or from an instance of 'Enum'.
--
-- >>> LT^.from enum
-- 0
--
-- >>> 97^.enum :: Char
-- 'a'
--
-- Note: this is only an isomorphism from the numeric range actually used
-- and it is a bit of a pleasant fiction, since there are questionable
-- 'Enum' instances for 'Double', and 'Float' that exist solely for
-- @[1.0 .. 4.0]@ sugar and the instances for those and 'Integer' don't
-- cover all values in their range.
enum :: Enum a => Iso' Int a
enum = iso toEnum fromEnum
{-# INLINE enum #-}

-- | This can be used to lift any 'Iso' into an arbitrary 'Functor'.
mapping :: (Functor f, Functor g) => AnIso s t a b -> Iso (f s) (g t) (f a) (g b)
mapping k = withIso k $ \ sa bt -> iso (fmap sa) (fmap bt)
{-# INLINE mapping #-}

-- | If @v@ is an element of a type @a@, and @a'@ is @a@ sans the element @v@, then @'non' v@ is an isomorphism from
-- @'Maybe' a'@ to @a@.
--
-- @
-- 'non' ≡ 'non'' '.' 'only'
-- @
--
-- Keep in mind this is only a real isomorphism if you treat the domain as being @'Maybe' (a sans v)@.
--
-- This is practically quite useful when you want to have a 'Data.Map.Map' where all the entries should have non-zero values.
--
-- >>> Map.fromList [("hello",1)] & at "hello" . non 0 +~ 2
-- fromList [("hello",3)]
--
-- >>> Map.fromList [("hello",1)] & at "hello" . non 0 -~ 1
-- fromList []
--
-- >>> Map.fromList [("hello",1)] ^. at "hello" . non 0
-- 1
--
-- >>> Map.fromList [] ^. at "hello" . non 0
-- 0
--
-- This combinator is also particularly useful when working with nested maps.
--
-- /e.g./ When you want to create the nested 'Data.Map.Map' when it is missing:
--
-- >>> Map.empty & at "hello" . non Map.empty . at "world" ?~ "!!!"
-- fromList [("hello",fromList [("world","!!!")])]
--
-- and when have deleting the last entry from the nested 'Data.Map.Map' mean that we
-- should delete its entry from the surrounding one:
--
-- >>> Map.fromList [("hello",Map.fromList [("world","!!!")])] & at "hello" . non Map.empty . at "world" .~ Nothing
-- fromList []
--
-- It can also be used in reverse to exclude a given value:
--
-- >>> non 0 # rem 10 4
-- Just 2
--
-- >>> non 0 # rem 10 5
-- Nothing
non :: Eq a => a -> Iso' (Maybe a) a
non a = non' $ only a
{-# INLINE non #-}

-- | @'non'' p@ generalizes @'non' (p # ())@ to take any unit 'Prism'
--
-- This function generates an isomorphism between @'Maybe' (a | 'isn't' p a)@ and @a@.
--
-- >>> Map.singleton "hello" Map.empty & at "hello" . non' _Empty . at "world" ?~ "!!!"
-- fromList [("hello",fromList [("world","!!!")])]
--
-- >>> Map.fromList [("hello",Map.fromList [("world","!!!")])] & at "hello" . non' _Empty . at "world" .~ Nothing
-- fromList []
non' :: APrism' a () -> Iso' (Maybe a) a
non' p = iso (fromMaybe def) go where
  def                           = review (clonePrism p) ()
  go b | has (clonePrism p) b   = Nothing
       | otherwise              = Just b
{-# INLINE non' #-}

-- | @'anon' a p@ generalizes @'non' a@ to take any value and a predicate.
--
-- This function assumes that @p a@ holds @'True'@ and generates an isomorphism between @'Maybe' (a | 'not' (p a))@ and @a@.
--
-- >>> Map.empty & at "hello" . anon Map.empty Map.null . at "world" ?~ "!!!"
-- fromList [("hello",fromList [("world","!!!")])]
--
-- >>> Map.fromList [("hello",Map.fromList [("world","!!!")])] & at "hello" . anon Map.empty Map.null . at "world" .~ Nothing
-- fromList []
anon :: a -> (a -> Bool) -> Iso' (Maybe a) a
anon a p = iso (fromMaybe a) go where
  go b | p b       = Nothing
       | otherwise = Just b
{-# INLINE anon #-}

-- | The canonical isomorphism for currying and uncurrying a function.
--
-- @
-- 'curried' = 'iso' 'curry' 'uncurry'
-- @
--
-- >>> (fst^.curried) 3 4
-- 3
--
-- >>> view curried fst 3 4
-- 3
curried :: Iso ((a,b) -> c) ((d,e) -> f) (a -> b -> c) (d -> e -> f)
curried = iso curry uncurry
{-# INLINE curried #-}

-- | The canonical isomorphism for uncurrying and currying a function.
--
-- @
-- 'uncurried' = 'iso' 'uncurry' 'curry'
-- @
--
-- @
-- 'uncurried' = 'from' 'curried'
-- @
--
-- >>> ((+)^.uncurried) (1,2)
-- 3
uncurried :: Iso (a -> b -> c) (d -> e -> f) ((a,b) -> c) ((d,e) -> f)
uncurried = iso uncurry curry
{-# INLINE uncurried #-}

-- | The isomorphism for flipping a function.
--
-- >>>((,)^.flipped) 1 2
-- (2,1)
flipped :: Iso (a -> b -> c) (a' -> b' -> c') (b -> a -> c) (b' -> a' -> c')
flipped = iso flip flip
{-# INLINE flipped #-}

-- |
-- @
-- 'swapped' '.' 'swapped' ≡ 'id'
-- 'first' f '.' 'swapped' = 'swapped' '.' 'second' f
-- 'second' g '.' 'swapped' = 'swapped' '.' 'first' g
-- 'bimap' f g '.' 'swapped' = 'swapped' '.' 'bimap' g f
-- @
--
-- >>> (1,2)^.swapped
-- (2,1)
swapped :: Swap p => Iso (p a b) (p c d) (p b a) (p d c)
swapped = iso swap swap
{-# INLINE swapped #-}

-- | An 'Iso' between the lazy variant of a structure and its strict
-- counterpart.
--
-- @
-- 'strict' = 'from' 'lazy'
-- @
strict :: Strict lazy strict => Iso' lazy strict
strict = iso toStrict toLazy
{-# INLINE strict #-}

pattern Strict :: Strict s t => t -> s
pattern Strict a <- (view strict -> a) where
  Strict a = review strict a

pattern Lazy :: Strict t s => t -> s
pattern Lazy a <- (view lazy -> a) where
  Lazy a = review lazy a

pattern Swapped :: Swap p => p b a -> p a b
pattern Swapped a <- (view swapped -> a) where
  Swapped a = review swapped a

pattern Reversed :: Reversing t => t -> t
pattern Reversed a <- (view reversed -> a) where
  Reversed a = review reversed a

-- | An 'Iso' between the strict variant of a structure and its lazy
-- counterpart.
--
-- @
-- 'lazy' = 'from' 'strict'
-- @
--
lazy :: Strict lazy strict => Iso' strict lazy
lazy = iso toLazy toStrict
{-# INLINE lazy #-}

-- | An 'Iso' between a list, 'ByteString', 'Text' fragment, etc. and its reversal.
--
-- >>> "live" ^. reversed
-- "evil"
--
-- >>> "live" & reversed %~ ('d':)
-- "lived"
reversed :: Reversing a => Iso' a a
reversed = involuted Iso.reversing

-- | Given a function that is its own inverse, this gives you an 'Iso' using it in both directions.
--
-- @
-- 'involuted' ≡ 'Control.Monad.join' 'iso'
-- @
--
-- >>> "live" ^. involuted reverse
-- "evil"
--
-- >>> "live" & involuted reverse %~ ('d':)
-- "lived"
involuted :: (a -> a) -> Iso' a a
involuted a = iso a a
{-# INLINE involuted #-}

pattern List :: Exts.IsList l => [Exts.Item l] -> l
pattern List a <- (Exts.toList -> a) where
  List a = Exts.fromList a

------------------------------------------------------------------------------
-- Magma
------------------------------------------------------------------------------

-- | This isomorphism can be used to inspect a 'Traversal' to see how it associates
-- the structure and it can also be used to bake the 'Traversal' into a 'Magma' so
-- that you can traverse over it multiple times.
magma :: LensLike (Mafic a b) s t a b -> Iso s u (Magma Int t b a) (Magma j u c c)
magma l = iso (runMafic `rmap` l sell) runMagma
{-# INLINE magma #-}

-- | This isomorphism can be used to inspect an 'IndexedTraversal' to see how it associates
-- the structure and it can also be used to bake the 'IndexedTraversal' into a 'Magma' so
-- that you can traverse over it multiple times with access to the original indices.
imagma :: Over (Indexed i) (Molten i a b) s t a b -> Iso s t' (Magma i t b a) (Magma j t' c c)
imagma l = iso (runMolten #. l sell) (iextract .# Molten)
{-# INLINE imagma #-}

------------------------------------------------------------------------------
-- Contravariant
------------------------------------------------------------------------------

-- | Lift an 'Iso' into a 'Contravariant' functor.
--
-- @
-- contramapping :: 'Contravariant' f => 'Iso' s t a b -> 'Iso' (f a) (f b) (f s) (f t)
-- contramapping :: 'Contravariant' f => 'Iso'' s a -> 'Iso'' (f a) (f s)
-- @
contramapping :: Contravariant f => AnIso s t a b -> Iso (f a) (f b) (f s) (f t)
contramapping f = withIso f $ \ sa bt -> iso (contramap sa) (contramap bt)
{-# INLINE contramapping #-}

------------------------------------------------------------------------------
-- Profunctor
------------------------------------------------------------------------------

-- | Lift two 'Iso's into both arguments of a 'Profunctor' simultaneously.
--
-- @
-- dimapping :: 'Profunctor' p => 'Iso' s t a b -> 'Iso' s' t' a' b' -> 'Iso' (p a s') (p b t') (p s a') (p t b')
-- dimapping :: 'Profunctor' p => 'Iso'' s a -> 'Iso'' s' a' -> 'Iso'' (p a s') (p s a')
-- @
dimapping :: (Profunctor p, Profunctor q) => AnIso s t a b -> AnIso s' t' a' b' -> Iso (p a s') (q b t') (p s a') (q t b')
dimapping f g = withIso f $ \ sa bt -> withIso g $ \ s'a' b't' ->
  iso (dimap sa s'a') (dimap bt b't')
{-# INLINE dimapping #-}

-- | Lift an 'Iso' contravariantly into the left argument of a 'Profunctor'.
--
-- @
-- lmapping :: 'Profunctor' p => 'Iso' s t a b -> 'Iso' (p a x) (p b y) (p s x) (p t y)
-- lmapping :: 'Profunctor' p => 'Iso'' s a -> 'Iso'' (p a x) (p s x)
-- @
lmapping :: (Profunctor p, Profunctor q) => AnIso s t a b -> Iso (p a x) (q b y) (p s x) (q t y)
lmapping f = withIso f $ \ sa bt -> iso (lmap sa) (lmap bt)
{-# INLINE lmapping #-}

-- | Lift an 'Iso' covariantly into the right argument of a 'Profunctor'.
--
-- @
-- rmapping :: 'Profunctor' p => 'Iso' s t a b -> 'Iso' (p x s) (p y t) (p x a) (p y b)
-- rmapping :: 'Profunctor' p => 'Iso'' s a -> 'Iso'' (p x s) (p x a)
-- @
rmapping :: (Profunctor p, Profunctor q) => AnIso s t a b -> Iso (p x s) (q y t) (p x a) (q y b)
rmapping g = withIso g $ \ sa bt -> iso (rmap sa) (rmap bt)
{-# INLINE rmapping #-}

------------------------------------------------------------------------------
-- Bifunctor
------------------------------------------------------------------------------

-- | Lift two 'Iso's into both arguments of a 'Bifunctor'.
--
-- @
-- bimapping :: 'Bifunctor' p => 'Iso' s t a b -> 'Iso' s' t' a' b' -> 'Iso' (p s s') (p t t') (p a a') (p b b')
-- bimapping :: 'Bifunctor' p => 'Iso'' s a -> 'Iso'' s' a' -> 'Iso'' (p s s') (p a a')
-- @
bimapping :: (Bifunctor f, Bifunctor g) => AnIso s t a b -> AnIso s' t' a' b' -> Iso (f s s') (g t t') (f a a') (g b b')
bimapping f g = withIso f $ \ sa bt -> withIso g $ \s'a' b't' ->
  iso (bimap sa s'a') (bimap bt b't')
{-# INLINE bimapping #-}

-- | Lift an 'Iso' into the first argument of a 'Bifunctor'.
--
-- @
-- firsting :: 'Bifunctor' p => 'Iso' s t a b -> 'Iso' (p s x) (p t y) (p a x) (p b y)
-- firsting :: 'Bifunctor' p => 'Iso'' s a -> 'Iso'' (p s x) (p a x)
-- @
firsting :: (Bifunctor f, Bifunctor g) => AnIso s t a b -> Iso (f s x) (g t y) (f a x) (g b y)
firsting p = withIso p $ \ sa bt -> iso (first sa) (first bt)
{-# INLINE firsting #-}

-- | Lift an 'Iso' into the second argument of a 'Bifunctor'. This is
-- essentially the same as 'mapping', but it takes a 'Bifunctor p'
-- constraint instead of a 'Functor (p a)' one.
--
-- @
-- seconding :: 'Bifunctor' p => 'Iso' s t a b -> 'Iso' (p x s) (p y t) (p x a) (p y b)
-- seconding :: 'Bifunctor' p => 'Iso'' s a -> 'Iso'' (p x s) (p x a)
-- @
seconding :: (Bifunctor f, Bifunctor g) => AnIso s t a b -> Iso (f x s) (g y t) (f x a) (g y b)
seconding p = withIso p $ \ sa bt -> iso (second sa) (second bt)
{-# INLINE seconding #-}

-- | Data types that are representationally equal are isomorphic.
--
-- This is only available on GHC 7.8+
--
-- @since 4.13
coerced :: forall s t a b. (Coercible s a, Coercible t b) => Iso s t a b
coerced l = rmap (fmap coerce) l .# coerce
{-# INLINE coerced #-}

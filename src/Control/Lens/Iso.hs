{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_bytestring
#define MIN_VERSION_bytestring(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Iso
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
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
  , runIso
  -- * Working with isomorphisms
  , au
  , auf
  , under
  , mapping
  -- ** Common Isomorphisms
  , simple
  , non
  , anon
  , enum
  , curried, uncurried
  , flipped
  , Strict(..)
  -- * Profunctors
  , Profunctor(..)
  ) where

import Control.Lens.Internal
import Control.Lens.Type
import Data.ByteString as StrictB
import Data.ByteString.Lazy as LazyB
import Data.Text as StrictT
import Data.Text.Lazy as LazyT
import Data.Maybe
import Data.Profunctor
#ifndef SAFE
import Unsafe.Coerce
#endif

{-# ANN module "HLint: ignore Use on" #-}

-- $setup
-- >>> import Control.Lens
-- >>> import Data.Map as Map
-- >>> import Data.Foldable
-- >>> import Data.Monoid

----------------------------------------------------------------------------
-- Isomorphisms
-----------------------------------------------------------------------------

-- | When you see this as an argument to a function, it expects an 'Iso'.
type AnIso s t a b = Exchange a b a (Mutator b) -> Exchange a b s (Mutator t)

-- | A 'Simple' 'AnIso'.
type AnIso' s a = AnIso s s a a


-- | Build a simple isomorphism from a pair of inverse functions
--
-- @
-- 'Control.Lens.Getter.view' ('iso' f g) ≡ f
-- 'Control.Lens.Getter.view' ('Control.Lens.Iso.from' ('iso' f g)) ≡ g
-- 'Control.Lens.Setter.set' ('iso' f g) h ≡ g '.' h '.' f
-- 'Control.Lens.Setter.set' ('Control.Lens.Iso.from' ('iso' f g)) h ≡ f '.' h '.' g
-- @
iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

----------------------------------------------------------------------------
-- Consuming Isomorphisms
-----------------------------------------------------------------------------

-- | Invert an isomorphism.
--
-- @'from' ('from' l) ≡ l@
from :: AnIso s t a b -> Iso b a t s
from k = case runIso k of
  (sa, bt) -> iso bt sa
{-# INLINE from #-}

-- | Convert from 'AnIso' back to any 'Iso'.
--
-- This is useful when you need to store an isomorphism as a data type inside a container
-- and later reconstitute it as an overloaded function.
--
-- See 'cloneLens' or 'Control.Lens.Traversal.cloneTraversal' for more information on why you might want to do this.
cloneIso :: AnIso s t a b -> Iso s t a b
cloneIso k = case runIso k of
  (sa, bt) -> iso sa bt
{-# INLINE cloneIso #-}

-----------------------------------------------------------------------------
-- Isomorphisms families as Lenses
-----------------------------------------------------------------------------

-- | Safely decompose 'AnIso'
--
-- @'cloneIso' ≡ 'uncurry' 'iso' '.' 'runIso'@
--
-- @'from' ≡ 'uncurry' ('flip' 'iso') '.' 'runIso'@
runIso :: AnIso s t a b -> (s -> a, b -> t)
#ifdef SAFE
runIso ai = case runExchange $ ai $ Exchange (id, Mutator) of
  (sa, bt) -> (sa, runMutator #. bt)
#else
runIso ai = unsafeCoerce (runExchange $ ai $ Exchange (id, Mutator))
#endif

{-# INLINE runIso #-}

-- | Based on 'Control.Lens.Wrapped.ala' from Conor McBride's work on Epigram.
--
-- This version is generalized to accept any 'Iso', not just a @newtype@.
--
-- >>> au (wrapping Sum) foldMap [1,2,3,4]
-- 10
au :: AnIso s t a b -> ((s -> a) -> e -> b) -> e -> t
au k = case runIso k of
  (sa, bt) -> \ f e -> bt (f sa e)
{-# INLINE au #-}

-- |
-- Based on @ala'@ from Conor McBride's work on Epigram.
--
-- This version is generalized to accept any 'Iso', not just a @newtype@.
--
-- For a version you pass the name of the @newtype@ constructor to, see 'Control.Lens.Wrapped.alaf'.
--
-- Mnemonically, the German /auf/ plays a similar role to /à la/, and the combinator
-- is 'au' with an extra function argument.
--
-- >>> auf (wrapping Sum) (foldMapOf both) Prelude.length ("hello","world")
-- 10
auf :: AnIso s t a b -> ((r -> a) -> e -> b) -> (r -> s) -> e -> t
auf k = case runIso k of
  (sa, bt) -> \ f g e -> bt (f (sa . g) e)
{-# INLINE auf #-}

-- | The opposite of working 'Control.Lens.Setter.over' a 'Setter' is working 'under' an isomorphism.
--
-- @'under' ≡ 'Control.Lens.Setter.over' '.' 'from'@
--
-- @'under' :: 'Iso' s t a b -> (s -> t) -> a -> b@
under :: AnIso s t a b -> (t -> s) -> b -> a
under k = case runIso k of
  (sa, bt) -> \ts -> sa . ts . bt
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
mapping :: Functor f => AnIso s t a b -> Iso (f s) (f t) (f a) (f b)
mapping k = case runIso k of
  (sa, bt) -> iso (fmap sa) (fmap bt)
{-# INLINE mapping #-}

-- | Composition with this isomorphism is occasionally useful when your 'Lens',
-- 'Control.Lens.Traversal.Traversal' or 'Iso' has a constraint on an unused
-- argument to force that argument to agree with the
-- type of a used argument and avoid @ScopedTypeVariables@ or other ugliness.
simple :: Iso' a a
simple = id
{-# INLINE simple #-}

-- | If @v@ is an element of a type @a@, and @a'@ is @a@ sans the element @v@, then @'non' v@ is an isomorphism from
-- @'Maybe' a'@ to @a@.
--
-- Keep in mind this is only a real isomorphism if you treat the domain as being @'Maybe' (a sans v)@
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
-- >>> fromList [("hello",fromList [("world","!!!")])] & at "hello" . non Map.empty . at "world" .~ Nothing
-- fromList []
non :: Eq a => a -> Iso' (Maybe a) a
non a = anon a (a==)
{-# INLINE non #-}

-- | @'anon' a p@ generalizes @'non' a@ to take any value and a predicate.
--
-- This function assumes that @p a@ holds @'True'@ and generates an isomorphism between @'Maybe' (a | 'not' (p a))@ and @a@
--
-- >>> Map.empty & at "hello" . anon Map.empty Map.null . at "world" ?~ "!!!"
-- fromList [("hello",fromList [("world","!!!")])]
--
-- >>> fromList [("hello",fromList [("world","!!!")])] & at "hello" . anon Map.empty Map.null . at "world" .~ Nothing
-- fromList []
anon :: a -> (a -> Bool) -> Iso' (Maybe a) a
anon a p = iso (fromMaybe a) go where
  go b | p b       = Nothing
       | otherwise = Just b
{-# INLINE anon #-}

-- | The canonical isomorphism for currying and uncurrying a function.
--
-- @'curried' = 'iso' 'curry' 'uncurry'@
curried :: Iso ((a,b) -> c) ((d,e) -> f) (a -> b -> c) (d -> e -> f)
curried = iso curry uncurry
{-# INLINE curried #-}

-- | The canonical isomorphism for uncurrying and currying a function.
--
-- @'uncurried' = 'iso' 'uncurry' 'curry'@
--
-- @'uncurried' = 'from' 'curried'@
uncurried :: Iso (a -> b -> c) (d -> e -> f) ((a,b) -> c) ((d,e) -> f)
uncurried = iso uncurry curry
{-# INLINE uncurried #-}

-- | The isomorphism for flipping a function.
flipped :: Iso (a -> b -> c) (a' -> b' -> c') (b -> a -> c) (b' -> a' -> c')
flipped = iso flip flip
{-# INLINE flipped #-}

-- | Ad hoc conversion between \"strict\" and \"lazy\" versions of a structure,
-- such as 'StrictT.Text' or 'StrictB.ByteString'.
class Strict s a | s -> a, a -> s where
  strict :: Iso' s a

instance Strict LazyB.ByteString StrictB.ByteString where
#if MIN_VERSION_bytestring(0,10,0)
  strict = iso LazyB.toStrict LazyB.fromStrict
#else
  strict = iso (StrictB.concat . LazyB.toChunks) (LazyB.fromChunks . return)
#endif
  {-# INLINE strict #-}

instance Strict LazyT.Text StrictT.Text where
  strict = iso LazyT.toStrict LazyT.fromStrict
  {-# INLINE strict #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_bytestring
#define MIN_VERSION_bytestring(x,y,z)
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
    Iso
  -- * Isomorphism Construction
  , Isomorphic(iso)
  , AnIso(..)
  -- * Consuming Isomorphisms
  , from
  , cloneIso
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
  , Strict(..)
  -- * Storing Isomorphisms
  , ReifiedIso(..)
  -- * Simplicity
  , SimpleIso
  , SimpleReifiedIso
  ) where

import Control.Lens.Classes (Isomorphic(..))
import Control.Lens.Internal (AnIso(..))
import Control.Lens.Type (Simple)
import Data.ByteString as StrictB
import Data.ByteString.Lazy as LazyB
import Data.Text as StrictT (Text)
import Data.Text.Lazy as LazyT (Text, toStrict, fromStrict)
import Data.Maybe (fromMaybe)

-- $setup
-- >>> import Control.Lens
-- >>> import Data.Map as Map
-- >>> import Data.Foldable
-- >>> import Data.Monoid

----------------------------------------------------------------------------
-- Consuming Isomorphisms
-----------------------------------------------------------------------------

-- | Invert an isomorphism.
--
-- @'from' ('from' l) ≡ l@
from :: AnIso s t a b -> Iso b a t s
from (Iso sa bt) = iso bt sa
{-# INLINE from #-}

-- | Convert from an 'Isomorphism' back to any 'Isomorphic' value.
--
-- This is useful when you need to store an isomoprhism as a data type inside a container
-- and later reconstitute it as an overloaded function.
--
-- See 'cloneLens' or 'Control.Lens.Traversal.cloneTraversal' for more information on why you might want to do this.
cloneIso :: AnIso s t a b -> Iso s t a b
cloneIso (Iso sa bt) = iso sa bt
{-# INLINE cloneIso #-}

-----------------------------------------------------------------------------
-- Isomorphisms families as Lenses
-----------------------------------------------------------------------------

-- | Isomorphism families can be composed with other lenses using ('.') and 'id'.
type Iso s t a b = forall r. (Isomorphic r, S r ~ s, T r ~ t, A r ~ a, B r ~ b) => r

-- |
-- @type 'SimpleIso' = 'Control.Lens.Type.Simple' 'Iso'@
type SimpleIso s a = Iso s s a a

-- | Based on 'Control.Lens.Wrapped.ala' from Conor McBride's work on Epigram.
--
-- This version is generalized to accept any 'Iso', not just a @newtype@.
--
-- >>> au (wrapping Sum) foldMap [1,2,3,4]
-- 10
au :: AnIso s t a b -> ((s -> a) -> e -> b) -> e -> t
au (Iso sa bt) f e = bt (f sa e)
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
auf (Iso sa bt) f g e = bt (f (sa . g) e)
{-# INLINE auf #-}

-- | The opposite of working 'over' a Setter is working 'under' an Isomorphism.
--
-- @'under' ≡ 'over' '.' 'from'@
--
-- @'under' :: 'Iso' s t a b -> (s -> t) -> a -> b@
under :: AnIso s t a b -> (t -> s) -> b -> a
under (Iso sa bt) ts b = sa (ts (bt b))
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
enum :: Enum a => Simple Iso Int a
enum = iso toEnum fromEnum
{-# INLINE enum #-}

-- | This can be used to lift any 'SimpleIso' into an arbitrary functor.
mapping :: Functor f => AnIso s t a b -> Iso (f s) (f t) (f a) (f b)
mapping (Iso sa bt) = iso (fmap sa) (fmap bt)
{-# INLINE mapping #-}

-- | Composition with this isomorphism is occasionally useful when your 'Lens',
-- 'Control.Lens.Traversal.Traversal' or 'Iso' has a constraint on an unused
-- argument to force that argument to agree with the
-- type of a used argument and avoid @ScopedTypeVariables@ or other ugliness.
simple :: Simple Iso a a
simple = iso id id
{-# INLINE simple #-}

-- | If @v@ is an element of a type @a@, and @a'@ is @a@ sans the element @v@, then @non v@ is an isomorphism from
-- @Maybe a'@ to @a@.
--
-- Keep in mind this is only a real isomorphism if you treat the domain as being @'Maybe' (a sans v)@
--
-- This is practically quite useful when you want to have a map where all the entries should have non-zero values.
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
-- /e.g./ When you want to create the nested map when it is missing:
--
-- >>> Map.empty & at "hello" . non Map.empty . at "world" ?~ "!!!"
-- fromList [("hello",fromList [("world","!!!")])]
--
-- and when have deleting the last entry from the nested map mean that we
-- should delete its entry from the surrounding one:
--
-- >>> fromList [("hello",fromList [("world","!!!")])] & at "hello" . non Map.empty . at "world" .~ Nothing
-- fromList []
non :: Eq a => a -> Simple Iso (Maybe a) a
non a = anon a (a==)
{-# INLINE non #-}

-- | @'anon' a p@ generalizes @'non' a@ to take any value and a predicate.
--
-- This function assumes that @p a@ holds @True@ and generates an isomorphism between @'Maybe' (a | not (p a))@ and @a@
--
-- >>> Map.empty & at "hello" . anon Map.empty Map.null . at "world" ?~ "!!!"
-- fromList [("hello",fromList [("world","!!!")])]
--
-- >>> fromList [("hello",fromList [("world","!!!")])] & at "hello" . anon Map.empty Map.null . at "world" .~ Nothing
-- fromList []
anon :: a -> (a -> Bool) -> Simple Iso (Maybe a) a
anon a p = iso (fromMaybe a) go where
  go b | p b       = Nothing
       | otherwise = Just b
{-# INLINE anon #-}

-- | The canonical isomorphism for currying and uncurrying function.
--
-- @'curried' = 'iso' 'curry' 'uncurry'@
curried :: Iso ((a,b) -> c) ((d,e) -> f) (a -> b -> c) (d -> e -> f)
curried = iso curry uncurry
{-# INLINE curried #-}

-- | The canonical isomorphism for uncurrying and currying function.
--
-- @'uncurried' = 'iso' 'uncurry' 'curry'@
-- @'uncurried' = 'from' 'curried'@
uncurried :: Iso (a -> b -> c) (d -> e -> f) ((a,b) -> c) ((d,e) -> f)
uncurried = iso uncurry curry
{-# INLINE uncurried #-}

-- | Ad hoc conversion between \"strict\" and \"lazy\" versions of a structure,
-- such as 'StrictT.Text' or 'StrictB.ByteString'.
class Strict s t a b | s -> a, a -> s, b -> t, t -> b, s b -> a t, a t -> s b where
  strict :: Iso s t a b

instance Strict LazyB.ByteString LazyB.ByteString StrictB.ByteString StrictB.ByteString where
#if MIN_VERSION_bytestring(0,10,0)
  strict = iso LazyB.toStrict LazyB.fromStrict
#else
  strict = iso (StrictB.concat . LazyB.toChunks) (LazyB.fromChunks . return)
#endif
  {-# INLINE strict #-}

instance Strict LazyT.Text LazyT.Text StrictT.Text StrictT.Text where
  strict = iso LazyT.toStrict LazyT.fromStrict
  {-# INLINE strict #-}

-----------------------------------------------------------------------------
-- Reifying Isomorphisms
-----------------------------------------------------------------------------

-- | Useful for storing isomorphisms in containers.
newtype ReifiedIso s t a b = ReifyIso { reflectIso :: Iso s t a b }

-- | @type 'SimpleReifiedIso' = 'Control.Lens.Type.Simple' 'ReifiedIso'@
type SimpleReifiedIso s a = ReifiedIso s s a a

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
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
  , Isomorphic(..)
  , iso
  -- * Consuming Isomorphisms
  , from
  , via
  , Isomorphism
  -- * Working with isomorphisms
  , ala
  , auf
  , under
  , mapping
  -- ** Common Isomorphisms
  , _const
  , identity
  , simple
  , non
  , enum
  -- * Storing Isomorphisms
  , ReifiedIso(..)
  -- * Simplicity
  , SimpleIso
  , SimpleReifiedIso
  ) where

import Control.Applicative
import Control.Category
import Control.Monad.Reader
import Control.Lens.Classes
import Control.Lens.Internal
import Control.Lens.Type
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Prelude hiding ((.),id)
import Unsafe.Coerce

-- $setup
-- >>> import Control.Lens
-- >>> import Data.Map as Map

----------------------------------------------------------------------------
-- Consuming Isomorphisms
-----------------------------------------------------------------------------

type Isomorphism s t a b = Isos (a -> Mutator b) (s -> Mutator t)

-- | Invert an isomorphism.
--
-- Note to compose an isomorphism and receive an isomorphism in turn you'll need to use
-- 'Control.Category.Category'
--
-- @'from' ('from' l) ≡ l@
--
-- If you imported 'Control.Category..' from @Control.Category@, then:
--
-- @'from' l '.' 'from' r ≡ 'from' (r '.' l)@
from :: (Isomorphic k, Functor f) => Isomorphism s t a b -> k (s -> f t) (a -> f b)
from (Isos sa as tb bt) = isos as sa (unsafeCoerce bt) (unsafeCoerce tb)
{-# INLINE from #-}

-- | Convert from an 'Isomorphism' back to any 'Isomorphic' value.
--
-- This is useful when you need to store an isomoprhism as a data type inside a container
-- and later reconstitute it as an overloaded function.
via :: (Isomorphic k, Functor f) => Isomorphism s t a b -> k (a -> f b) (s -> f t)
via (Isos sa as tb bt) = isos sa as (unsafeCoerce tb) (unsafeCoerce bt)
{-# INLINE via #-}

-----------------------------------------------------------------------------
-- Isomorphisms families as Lenses
-----------------------------------------------------------------------------

-- | Isomorphism families can be composed with other lenses using either ('.') and 'id'
-- from the Prelude or from Control.Category. However, if you compose them
-- with each other using ('.') from the Prelude, they will be dumbed down to a
-- mere 'Lens'.
--
-- @
-- import Control.Category
-- import Prelude hiding (('Prelude..'),'Prelude.id')
-- @
--
-- @type 'Iso' s t a b = forall k f. ('Isomorphic' k, 'Functor' f) => 'Overloaded' k f s t a b@
type Iso s t a b = forall k f. (Isomorphic k, Functor f) => k (a -> f b) (s -> f t)

-- |
-- @type 'SimpleIso' = 'Control.Lens.Type.Simple' 'Iso'@
type SimpleIso s a = Iso s s a a

-- | Build a simple isomorphism from a pair of inverse functions
--
--
-- @
-- 'view' ('iso' f g) ≡ f
-- 'view' ('from' ('iso' f g)) ≡ g
-- 'set' ('iso' f g) h ≡ g '.' h '.' f
-- 'set' ('from' ('iso' f g)) h ≡ f '.' h '.' g
-- @
--
-- @iso :: (s -> a) -> (a -> s) -> 'Control.Lens.Type.Simple' 'Iso' s a@
iso :: (Isomorphic k, Functor f) => (s -> a) -> (a -> s) -> k (a -> f a) (s -> f s)
iso sa as = isos sa as sa as
{-# INLINE iso #-}

-- | Based on @ala@ from Conor McBride's work on Epigram.
--
-- >>> :m + Data.Monoid.Lens Data.Foldable
-- >>> ala _sum foldMap [1,2,3,4]
-- 10
ala :: Isomorphism s t a b -> ((s -> a) -> e -> b) -> e -> t
ala (Isos sa _ _ bt) f e = unsafeCoerce bt (f sa e)
{-# INLINE ala #-}

-- |
-- Based on @ala'@ from Conor McBride's work on Epigram.
--
-- Mnemonically, the German /auf/ plays a similar role to /à la/, and the combinator
-- is 'ala' with an extra function argument.
auf :: Isomorphism s t a b -> ((r -> a) -> e -> b) -> (r -> s) -> e -> t
auf (Isos sa _ _ bt) f g e = unsafeCoerce bt (f (sa . g) e)
{-# INLINE auf #-}

-- | The opposite of working 'over' a Setter is working 'under' an Isomorphism.
--
-- @'under' ≡ 'over' '.' 'from'@
--
-- @'under' :: 'Iso' s t a b -> (s -> t) -> a -> b@
under :: Isomorphism s t a b -> (s -> t) -> a -> b
under (Isos _ as tb _) st a = unsafeCoerce tb (st (as a))
{-# INLINE under #-}

-----------------------------------------------------------------------------
-- Isomorphisms
-----------------------------------------------------------------------------

-- | This isomorphism can be used to wrap or unwrap a value in 'Identity'.
--
-- @
-- x^.identity ≡ 'Identity' x
-- 'Identity' x '^.' 'from' 'identity' ≡ x
-- @
identity :: Iso a b (Identity a) (Identity b)
identity = isos Identity runIdentity Identity runIdentity
{-# INLINE identity #-}

-- | This isomorphism can be used to wrap or unwrap a value in 'Const'
--
-- @
-- x '^.' '_const' ≡ 'Const' x
-- 'Const' x '^.' 'from' '_const' ≡ x
-- @
_const :: Iso a b (Const a c) (Const b d)
_const = isos Const getConst Const getConst
{-# INLINE _const #-}

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
mapping :: Functor f => Isos (a -> g b) (s -> g t) -> Iso (f s) (f t) (f a) (f b)
mapping (Isos sa as tb bt) = isos (fmap sa) (fmap as) (fmap (unsafeCoerce tb)) (fmap (unsafeCoerce bt))
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
non a = iso (fromMaybe a) go where
  go b | a == b    = Nothing
       | otherwise = Just b

-----------------------------------------------------------------------------
-- Reifying Isomorphisms
-----------------------------------------------------------------------------

-- | Useful for storing isomorphisms in containers.
newtype ReifiedIso s t a b = ReifyIso { reflectIso :: Iso s t a b }

-- | @type 'SimpleReifiedIso' = 'Control.Lens.Type.Simple' 'ReifiedIso'@
type SimpleReifiedIso s a = ReifiedIso s s a a

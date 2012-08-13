{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
  , iso
  , isos
  , au
  -- ** Combinators
  , via
  , from
  -- ** Common Isomorphisms
  , _const
  , identity
  -- * Implementation
  , Isomorphic(..)
  , Isomorphism(..)
  -- * Simplicity
  , SimpleIso
  ) where

import Control.Applicative
import Control.Category
import Control.Lens.Type
import Control.Lens.Getter
import Data.Functor.Identity
import Data.Typeable
import Prelude hiding ((.),id)

----------------------------------------------------------------------------
-- Isomorphism Implementation Details
-----------------------------------------------------------------------------

-- | Used to provide overloading of isomorphism application
--
-- This is a 'Category' with a canonical mapping to it from the
-- category of isomorphisms over Haskell types.
class Category k => Isomorphic k where
  -- | Build this morphism out of an isomorphism
  --
  -- The intention is that by using 'isomorphic', you can supply both halves of an
  -- isomorphism, but k can be instantiated to @(->)@, so you can freely use
  -- the resulting isomorphism as a function.
  isomorphic :: (a -> b) -> (b -> a) -> k a b

  -- | Map a morphism in the target category using an isomorphism between morphisms
  -- in Hask.
  isomap :: ((a -> b) -> c -> d) -> ((b -> a) -> d -> c) -> k a b -> k c d

instance Isomorphic (->) where
  isomorphic = const
  {-# INLINE isomorphic #-}
  isomap = const
  {-# INLINE isomap #-}

-- | A concrete data type for isomorphisms.
--
-- This lets you place an isomorphism inside a container without using @ImpredicativeTypes@.
data Isomorphism a b = Isomorphism (a -> b) (b -> a)
  deriving Typeable

instance Category Isomorphism where
  id = Isomorphism id id
  {-# INLINE id #-}
  Isomorphism bc cb . Isomorphism ab ba = Isomorphism (bc . ab) (ba . cb)
  {-# INLINE (.) #-}

instance Isomorphic Isomorphism where
  isomorphic = Isomorphism
  {-# INLINE isomorphic #-}
  isomap abcd badc (Isomorphism ab ba) = Isomorphism (abcd ab) (badc ba)
  {-# INLINE isomap #-}

-- | Invert an isomorphism.
--
-- Note to compose an isomorphism and receive an isomorphism in turn you'll need to use
-- 'Control.Category.Category'
--
-- > from (from l) = l
--
-- If you imported 'Control.Category..' from @Control.Category@, then:
--
-- > from l . from r = from (r . l)
from :: Isomorphic k => Isomorphism a b -> k b a
from (Isomorphism a b) = isomorphic b a
{-# INLINE from #-}
{-# SPECIALIZE from :: Isomorphism a b -> b -> a #-}
{-# SPECIALIZE from :: Isomorphism a b -> Isomorphism b a #-}

-- | Convert from an 'Isomorphism' back to any 'Isomorphic' value.
--
-- This is useful when you need to store an isomoprhism as a data type inside a container
-- and later reconstitute it as an overloaded function.
via :: Isomorphic k => Isomorphism a b -> k a b
via (Isomorphism a b) = isomorphic a b
{-# INLINE via #-}
{-# SPECIALIZE via :: Isomorphism a b -> a -> b #-}
{-# SPECIALIZE via :: Isomorphism a b -> Isomorphism a b #-}

-----------------------------------------------------------------------------
-- Isomorphisms families as Lenses
-----------------------------------------------------------------------------

-- | Isomorphim families can be composed with other lenses using either ('.') and 'id'
-- from the Prelude or from Control.Category. However, if you compose them
-- with each other using ('.') from the Prelude, they will be dumbed down to a
-- mere 'Lens'.
--
-- > import Control.Category
-- > import Prelude hiding ((.),id)
--
-- @type Iso a b c d = forall k f. ('Isomorphic' k, 'Functor' f) => 'Overloaded' k f a b c d@
type Iso a b c d = forall k f. (Isomorphic k, Functor f) => k (c -> f d) (a -> f b)

-- |
-- @type SimpleIso = 'Control.Lens.Type.Simple' 'Iso'@
type SimpleIso a b = Iso a a b b

-- | Build an isomorphism family from two pairs of inverse functions
--
-- @
-- 'view' ('isos' ac ca bd db) = ac
-- 'view' ('from' ('isos' ac ca bd db)) = ca
-- 'set' ('isos' ac ca bd db) cd = db . cd . ac
-- 'set' ('from' ('isos' ac ca bd db')) ab = bd . ab . ca
-- @
--
-- @isos :: (a -> c) -> (c -> a) -> (b -> d) -> (d -> b) -> 'Iso' a b c d@
isos :: (Isomorphic k, Functor f) => (a -> c) -> (c -> a) -> (b -> d) -> (d -> b) -> k (c -> f d) (a -> f b)
isos ac ca bd db = isomorphic
  (\cfd a -> db <$> cfd (ac a))
  (\afb c -> bd <$> afb (ca c))
{-# INLINE isos #-}
{-# SPECIALIZE isos :: Functor f => (a -> c) -> (c -> a) -> (b -> d) -> (d -> b) -> (c -> f d) -> a -> f b #-}
{-# SPECIALIZE isos :: Functor f => (a -> c) -> (c -> a) -> (b -> d) -> (d -> b) -> Isomorphism (c -> f d) (a -> f b) #-}

-- | Build a simple isomorphism from a pair of inverse functions
--
--
-- @
-- 'view' ('iso' f g) = f
-- 'view' ('from' ('iso' f g)) = g
-- 'set' ('isos' f g) h = g . h . f
-- 'set' ('from' ('iso' f g')) h = f . h . g
-- @
--
-- @iso :: (a -> b) -> (b -> a) -> 'Control.Lens.Type.Simple' 'Iso' a b@
iso :: (Isomorphic k, Functor f) => (a -> b) -> (b -> a) -> k (b -> f b) (a -> f a)
iso ab ba = isos ab ba ab ba
{-# INLINE iso #-}
{-# SPECIALIZE iso :: Functor f => (a -> b) -> (b -> a) -> (b -> f b) -> a -> f a #-}
{-# SPECIALIZE iso :: Functor f => (a -> b) -> (b -> a) -> Isomorphism (b -> f b) (a -> f a) #-}

-- | Based on @ala@ from Conor McBride's work on Epigram and @Control.Newtype@ from the
-- 'newtype package.
--
-- Mnemonically, /au/ is a French contraction of /à le/.
--
-- >>> :m + Control.Lens Data.Monoid.Lens Data.Foldable
-- >>> au _sum foldMap [1,2,3,4]
-- 10
au :: Simple Iso a b -> ((a -> b) -> e -> b) -> e -> a
au l f e = f (view l) e ^. from l

{-
under :: Setter a b c d -> (c -> d) -> a -> b
under = adjust

over :: Isomorphism (c -> Identity d) (a -> Identity b) -> (a -> b) -> c -> d
over = under . from
-}

-----------------------------------------------------------------------------
-- Isomorphisms
-----------------------------------------------------------------------------

-- | This isomorphism can be used to wrap or unwrap a value in 'Identity'.
--
-- @
-- x^.identity = 'Identity' x
-- 'Identity' x '^.' 'from' 'identity' = x
-- @
identity :: Iso a b (Identity a) (Identity b)
identity = isos Identity runIdentity Identity runIdentity
{-# INLINE identity #-}

-- | This isomorphism can be used to wrap or unwrap a value in 'Const'
--
-- @
-- x '^.' '_const' = 'Const' x
-- 'Const' x '^.' 'from' '_const' = x
-- @
_const :: Iso a b (Const a c) (Const b d)
_const = isos Const getConst Const getConst
{-# INLINE _const #-}

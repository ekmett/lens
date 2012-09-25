{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
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
  , (:<->)
  , iso
  , isos
  , ala
  , auf
  , under
  -- * Primitive isomorphisms
  , from
  , via
  , Isomorphism(..)
  , Isomorphic(..)
  -- ** Common Isomorphisms
  , _const
  , identity
  -- * Storing Isomorphisms
  , ReifiedIso(..)
  -- * Simplicity
  , SimpleIso
  , SimpleReifiedIso
  ) where

import Control.Applicative
import Control.Category
import Control.Lens.Getter
import Control.Lens.Internal
import Control.Lens.Isomorphic
import Control.Lens.Setter
import Control.Lens.Type
import Data.Functor.Identity
import Prelude hiding ((.),id)

-- $setup
-- >>> import Control.Lens

infixr 0 :<->

-----------------------------------------------------------------------------
-- Isomorphisms families as Lenses
-----------------------------------------------------------------------------

-- | Isomorphim families can be composed with other lenses using either ('.') and 'id'
-- from the Prelude or from Control.Category. However, if you compose them
-- with each other using ('.') from the Prelude, they will be dumbed down to a
-- mere 'Lens'.
--
-- @
-- import Control.Category
-- import Prelude hiding (('Prelude..'),'Prelude.id')
-- @
--
-- @type 'Iso' a b c d = forall k f. ('Isomorphic' k, 'Functor' f) => 'Overloaded' k f a b c d@
type Iso a b c d = forall k f. (Isomorphic k, Functor f) => k (c -> f d) (a -> f b)

-- |
-- @type 'SimpleIso' = 'Control.Lens.Type.Simple' 'Iso'@
type SimpleIso a b = Iso a a b b

-- | An commonly used infix alias for @'Control.Lens.Type.Simple' 'Iso'@
type a :<-> b = Iso a a b b

-- | Build an isomorphism family from two pairs of inverse functions
--
-- @
-- 'view' ('isos' ac ca bd db) ≡ ac
-- 'view' ('from' ('isos' ac ca bd db)) ≡ ca
-- 'set' ('isos' ac ca bd db) cd ≡ db '.' cd '.' ac
-- 'set' ('from' ('isos' ac ca bd db')) ab ≡ bd '.' ab '.' ca
-- @
--
-- @isos :: (a -> c) -> (c -> a) -> (b -> d) -> (d -> b) -> 'Iso' a b c d@
isos :: (Isomorphic k, Functor f) => (a -> c) -> (c -> a) -> (b -> d) -> (d -> b) -> k (c -> f d) (a -> f b)
isos ac ca bd db = isomorphic
  (\cfd a -> db <$> cfd (ac a))
  (\afb c -> bd <$> afb (ca c))
{-# INLINE isos #-}

-- | Build a simple isomorphism from a pair of inverse functions
--
--
-- @
-- 'view' ('iso' f g) ≡ f
-- 'view' ('from' ('iso' f g)) ≡ g
-- 'set' ('isos' f g) h ≡ g '.' h '.' f
-- 'set' ('from' ('iso' f g')) h ≡ f '.' h '.' g
-- @
--
-- @iso :: (a -> b) -> (b -> a) -> 'Control.Lens.Type.Simple' 'Iso' a b@
iso :: (Isomorphic k, Functor f) => (a -> b) -> (b -> a) -> k (b -> f b) (a -> f a)
iso ab ba = isos ab ba ab ba
{-# INLINE iso #-}

-- | Based on @ala@ from Conor McBride's work on Epigram.
--
-- >>> :m + Data.Monoid.Lens Data.Foldable
-- >>> ala _sum foldMap [1,2,3,4]
-- 10
ala :: Simple Iso a b -> ((a -> b) -> c -> b) -> c -> a
ala l f e = f (view l) e ^. from l
{-# INLINE ala #-}

-- |
-- Based on @ala'@ from Conor McBride's work on Epigram.
--
-- Mnemonically, the German /auf/ plays a similar role to /à la/, and the combinator
-- is 'ala' with an extra function argument.
auf :: Simple Iso a b -> ((d -> b) -> c -> b) -> (d -> a) -> c -> a
auf l f g e = f (view l . g) e ^. from l
{-# INLINE auf #-}

-- | The opposite of working 'over' a Setter is working 'under' an Isomorphism.
--
-- @'under' = 'over' '.' 'from'@
--
-- @'under' :: 'Iso' a b c d -> (a -> b) -> c -> d@
under :: Isomorphism (c -> Mutator d) (a -> Mutator b) -> (a -> b) -> c -> d
under = over . from
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

-----------------------------------------------------------------------------
-- Reifying Isomorphisms
-----------------------------------------------------------------------------

-- | Useful for storing isomorphisms in containers.
newtype ReifiedIso a b c d = ReifyIso { reflectIso :: Iso a b c d }

-- | @type 'SimpleReifiedIso' = 'Control.Lens.Type.Simple' 'ReifiedIso'@
type SimpleReifiedIso a b = ReifiedIso a a b b

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
  , simple
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

-- | This is a commonly-used infix alias for a @'Control.Lens.Type.Simple' 'Iso'@
type s :<-> a = Iso s s a a

-- | Build an isomorphism family from two pairs of inverse functions
--
-- @
-- 'view' ('isos' sa as tb bt) ≡ sa
-- 'view' ('from' ('isos' sa as tb bt)) ≡ as
-- 'set' ('isos' sa as tb bt) ab ≡ bt '.' ab '.' sa
-- 'set' ('from' ('isos' ac ca bd db')) ab ≡ bd '.' ab '.' ca
-- 'set' ('from' ('isos' sa as tb bt')) s t ≡ tb '.' st '.' as
-- @
--
-- @isos :: (s -> a) -> (a -> s) -> (t -> b) -> (b -> t) -> 'Iso' s t a b@
isos :: (Isomorphic k, Functor f) => (s -> a) -> (a -> s) -> (t -> b) -> (b -> t) -> k (a -> f b) (s -> f t)
isos sa as tb bt = isomorphic
  (\afb s -> bt <$> afb (sa s))
  (\sft a -> tb <$> sft (as a))
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
-- @iso :: (s -> a) -> (a -> s) -> 'Control.Lens.Type.Simple' 'Iso' s a@
iso :: (Isomorphic k, Functor f) => (s -> a) -> (a -> s) -> k (a -> f a) (s -> f s)
iso sa as = isos sa as sa as
{-# INLINE iso #-}

-- | Based on @ala@ from Conor McBride's work on Epigram.
--
-- >>> :m + Data.Monoid.Lens Data.Foldable
-- >>> ala _sum foldMap [1,2,3,4]
-- 10
ala :: Simple Iso s a -> ((s -> a) -> e -> a) -> e -> s
ala l f e = f (view l) e ^. from l
{-# INLINE ala #-}

-- |
-- Based on @ala'@ from Conor McBride's work on Epigram.
--
-- Mnemonically, the German /auf/ plays a similar role to /à la/, and the combinator
-- is 'ala' with an extra function argument.
auf :: Simple Iso s a -> ((b -> a) -> e -> a) -> (b -> s) -> e -> s
auf l f g e = f (view l . g) e ^. from l
{-# INLINE auf #-}

-- | The opposite of working 'over' a Setter is working 'under' an Isomorphism.
--
-- @'under' = 'over' '.' 'from'@
--
-- @'under' :: 'Iso' s t a b -> (s -> t) -> a -> b@
under :: Isomorphism (a -> Mutator b) (s -> Mutator t) -> (s -> t) -> a -> b
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


-- | Composition with this isomorphism is occasionally useful when your 'Lens',
-- 'Control.Lens.Traversal.Traversal' or 'Iso' has a constraint on an unused
-- argument to force that argument to agree with the
-- type of a used argument and avoid @ScopedTypeVariables@ or other ugliness.
simple :: Iso a b a b
simple = isos id id id id

-----------------------------------------------------------------------------
-- Reifying Isomorphisms
-----------------------------------------------------------------------------

-- | Useful for storing isomorphisms in containers.
newtype ReifiedIso s t a b = ReifyIso { reflectIso :: Iso s t a b }

-- | @type 'SimpleReifiedIso' = 'Control.Lens.Type.Simple' 'ReifiedIso'@
type SimpleReifiedIso s a = ReifiedIso s s a a

{-# LANGUAGE CPP #-}
-------------------------------------------------------------------------------
-- | This module provides conversion functions between the optics defined in
-- this library and 'Profunctor'-based optics.
--
-- The goal of these functions is to provide an interoperability layer between
-- the two styles of optics, and not to reimplement all the library in terms of
-- 'Profunctor' optics.

module Control.Lens.Profunctor
  ( -- * Profunctor optic
    OpticP

    -- * Conversion from Van Laarhoven optics
  , fromLens
  , fromIso
  , fromPrism
  , fromSetter
  , fromTraversal

    -- * Conversion to Van Laarhoven optics
  , toLens
  , toIso
  , toPrism
  , toSetter
  , toTraversal
  ) where

import Prelude ()

import Control.Lens.Internal.Prelude
import Control.Lens.Type (Optic, LensLike)
import Control.Lens.Internal.Context (Context (..), sell)
import Control.Lens.Internal.Profunctor (WrappedPafb (..))
import Control.Lens (ASetter, ATraversal, cloneTraversal, Settable)
import Data.Profunctor (Star (..))
import Data.Profunctor.Mapping (Mapping (..))
import Data.Profunctor.Traversing (Traversing (..))

-- | Profunctor optic.
type OpticP p s t a b = p a b -> p s t

--------------------------------------------------------------------------------
-- Conversion from Van Laarhoven optics
--------------------------------------------------------------------------------

-- | Converts a 'Control.Lens.Type.Lens' to a 'Profunctor'-based one.
--
-- @
-- 'fromLens' :: 'Control.Lens.Type.Lens' s t a b -> LensP s t a b
-- @
fromLens :: Strong p => LensLike (Context a b) s t a b -> OpticP p s t a b
fromLens l p =
  dimap
    (\s -> let Context f a = l sell s in (f, a))
    (uncurry id)
    (second' p)

-- | Converts a 'Control.Lens.Type.Iso' to a 'Profunctor'-based one.
--
-- @
-- 'fromIso' :: 'Control.Lens.Type.Iso' s t a b -> IsoP s t a b
-- @
fromIso :: Profunctor p => Optic p Identity s t a b -> OpticP p s t a b
fromIso p pab = rmap runIdentity (p (rmap Identity pab))

-- | Converts a 'Control.Lens.Type.Prism' to a 'Profunctor'-based one.
--
-- @
-- 'fromPrism' :: 'Control.Lens.Type.Prism' s t a b -> PrismP s t a b
-- @
fromPrism :: Choice p => Optic p Identity s t a b -> OpticP p s t a b
fromPrism p pab = rmap runIdentity (p (rmap Identity pab))

-- | Converts a 'Control.Lens.Type.Setter' to a 'Profunctor'-based one.
--
-- @
-- 'fromSetter' :: 'Control.Lens.Type.Setter' s t a b -> SetterP s t a b
-- @
fromSetter :: Mapping p => ASetter s t a b -> OpticP p s t a b
fromSetter s = roam s'
  where
    s' f = runIdentity . s (Identity . f)

-- | Converts a 'Control.Lens.Type.Traversal' to a 'Profunctor'-based one.
--
-- @
-- 'fromTraversal' :: 'Control.Lens.Type.Traversal' s t a b -> TraversalP s t a b
-- @
fromTraversal :: Traversing p => ATraversal s t a b -> OpticP p s t a b
fromTraversal l = wander (cloneTraversal l)

--------------------------------------------------------------------------------
-- Conversion to Van Laarhoven optics
--------------------------------------------------------------------------------

-- | Obtain a 'Control.Lens.Type.Prism' from a 'Profunctor'-based one.
--
-- @
-- 'toPrism' :: PrismP s t a b -> 'Control.Lens.Type.Prism' s t a b
-- @
toPrism :: (Choice p, Applicative f) => OpticP (WrappedPafb f p) s t a b -> Optic p f s t a b
toPrism p = unwrapPafb . p . WrapPafb

-- | Obtain a 'Control.Lens.Type.Iso' from a 'Profunctor'-based one.
--
-- @
-- 'toIso' :: IsoP s t a b -> 'Control.Lens.Type.Iso' s t a b
-- @
toIso :: (Profunctor p, Functor f) => OpticP (WrappedPafb f p) s t a b -> Optic p f s t a b
toIso p = unwrapPafb . p . WrapPafb

-- | Obtain a 'Control.Lens.Type.Lens' from a 'Profunctor'-based one.
--
-- @
-- 'toLens' :: LensP s t a b -> 'Control.Lens.Type.Lens' s t a b
-- @
toLens :: Functor f => OpticP (Star f) s t a b -> LensLike f s t a b
toLens p = runStar . p . Star

-- | Obtain a 'Control.Lens.Type.Setter' from a 'Profunctor'-based one.
--
-- @
-- 'toSetter' :: SetterP s t a b -> 'Control.Lens.Type.Setter' s t a b
-- @
toSetter :: Settable f => OpticP (Star f) s t a b -> LensLike f s t a b
toSetter p = runStar . p . Star

-- | Obtain a 'Control.Lens.Type.Traversal' from a 'Profunctor'-based one.
--
-- @
-- 'toTraversal' :: TraversalP s t a b -> 'Control.Lens.Type.Traversal' s t a b
-- @
toTraversal :: Applicative f => OpticP (Star f) s t a b -> LensLike f s t a b
toTraversal p = runStar . p . Star

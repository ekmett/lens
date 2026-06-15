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
import Control.Lens.Internal.Context (Context (..), runPretext, sell)
import Control.Lens.Internal.Profunctor (WrappedPafb (..))
import Control.Lens
  ( ALens, AnIso, APrism, ASetter, ATraversal
  , cloneTraversal, withIso, withPrism, Settable
  )
import Data.Profunctor (Star (..))
import Data.Profunctor.Mapping (Mapping (..))
import Data.Profunctor.Traversing (Traversing (..))

-- | Profunctor optic.
type OpticP p s t a b = p a b -> p s t

-- $setup
-- >>> import Control.Lens

--------------------------------------------------------------------------------
-- Conversion from Van Laarhoven optics
--------------------------------------------------------------------------------

-- | Converts a 'Control.Lens.Type.Lens' to a 'Profunctor'-based one.
--
-- The result is a 'Strong' profunctor optic: 'second'' carries the rebuild
-- function alongside the focus, so a single @k 'sell' s@ yields both the focus
-- and the setter in one pass.
--
-- >>> fromLens _1 (+1) (3, "hello")
-- (4,"hello")
fromLens :: Strong p => ALens s t a b -> OpticP p s t a b
fromLens k p = dimap project (uncurry id) (second' p)
  where
    project s = case runPretext (k sell s) (Context id) of
      Context bt a -> (bt, a)

-- | Converts an 'Control.Lens.Type.Iso' to a 'Profunctor'-based one.
--
-- An 'Control.Lens.Type.Iso' is just a pair of mutually inverse functions, so
-- this is exactly a 'dimap' — it adds no profunctor structure of its own.
--
-- >>> fromIso (iso fromEnum toEnum) succ 'a' :: Char
-- 'b'
fromIso :: Profunctor p => AnIso s t a b -> OpticP p s t a b
fromIso k = withIso k dimap

-- | Converts a 'Control.Lens.Type.Prism' to a 'Profunctor'-based one.
--
-- The result is a 'Choice' profunctor optic: 'right'' runs the profunctor only
-- on the matching branch, passing a non-match through to @t@ untouched.
--
-- >>> fromPrism _Just (+1) (Just 3)
-- Just 4
--
-- >>> fromPrism _Just (+1) Nothing
-- Nothing
fromPrism :: Choice p => APrism s t a b -> OpticP p s t a b
fromPrism k = withPrism k $ \bt seta ->
  dimap seta (either id bt) . right'

-- | Converts a 'Control.Lens.Type.Setter' to a 'Profunctor'-based one.
fromSetter :: Mapping p => ASetter s t a b -> OpticP p s t a b
fromSetter s = roam s'
  where
    s' f = runIdentity . s (Identity . f)

-- | Converts a 'Control.Lens.Type.Traversal' to a 'Profunctor'-based one.
fromTraversal :: Traversing p => ATraversal s t a b -> OpticP p s t a b
fromTraversal l = wander (cloneTraversal l)

--------------------------------------------------------------------------------
-- Conversion to Van Laarhoven optics
--------------------------------------------------------------------------------

-- | Obtain a 'Control.Lens.Type.Prism' from a 'Profunctor'-based one.
toPrism :: (Choice p, Applicative f) => OpticP (WrappedPafb f p) s t a b -> Optic p f s t a b
toPrism p = unwrapPafb . p . WrapPafb

-- | Obtain a 'Control.Lens.Type.Iso' from a 'Profunctor'-based one.
toIso :: (Profunctor p, Functor f) => OpticP (WrappedPafb f p) s t a b -> Optic p f s t a b
toIso p = unwrapPafb . p . WrapPafb

-- | Obtain a 'Control.Lens.Type.Lens' from a 'Profunctor'-based one.
toLens :: Functor f => OpticP (Star f) s t a b -> LensLike f s t a b
toLens p = runStar . p . Star

-- | Obtain a 'Control.Lens.Type.Setter' from a 'Profunctor'-based one.
toSetter :: Settable f => OpticP (Star f) s t a b -> LensLike f s t a b
toSetter p = runStar . p . Star

-- | Obtain a 'Control.Lens.Type.Traversal' from a 'Profunctor'-based one.
toTraversal :: Applicative f => OpticP (Star f) s t a b -> LensLike f s t a b
toTraversal p = runStar . p . Star

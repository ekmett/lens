{-# LANGUAGE RankNTypes #-}

module Control.Lens.Profunctor
  ( -- * Conversion from Van Laarhoven optics
    fromLens
  , fromPrism
  , fromSetter
  , fromTraversal
    -- * Conversion to Van Laarhoven optics
  , toLens
  , toPrism
  , toSetter
  , toTraversal
  ) where

import Control.Applicative (pure)
import Control.Lens.Type (Lens, Prism, Setter, Traversal)
import Control.Lens.Internal.Context (Context (..), sell)
import Data.Functor.Identity (Identity (..))
import Data.Profunctor (Choice (..), Profunctor (..), Strong (..), Star (..))
import Data.Profunctor.Mapping (Mapping (..))
import Data.Profunctor.Traversing (Traversing (..))

--------------------------------------------------------------------------------
-- Conversion from Van Laarhoven optics
--------------------------------------------------------------------------------

-- | Converts a 'Lens' to a 'Profunctor'-based one.
fromLens :: Strong p => Lens s t a b -> p a b -> p s t
fromLens l p =
  dimap
    (\s -> let (Context f a) = l sell s in (f, a))
    (uncurry id)
    (second' p)

-- | Converts a 'Prism' to a 'Profunctor'-based one.
fromPrism :: Choice p => Prism s t a b -> p a b -> p s t
fromPrism p pab = rmap runIdentity (p (rmap Identity pab))

-- | Converts a 'Setter' to a 'Profunctor'-based one.
fromSetter :: Mapping p => Setter s t a b -> p a b -> p s t
fromSetter s = roam s'
  where
    s' f = runIdentity . s (Identity . f)

-- | Converts a 'Traversal' to a 'Profunctor'-based one.
fromTraversal :: Traversing p => Traversal s t a b -> p a b -> p s t
fromTraversal = wander

--------------------------------------------------------------------------------
-- Conversion to Van Laarhoven optics
--------------------------------------------------------------------------------

newtype PrismWrapper f p a b = PrismWrapper { runPrismWrapper :: p a (f b) }

instance (Functor f, Profunctor p) => Profunctor (PrismWrapper f p) where
  dimap f g (PrismWrapper p) = PrismWrapper $ dimap f (fmap g) p

instance (Applicative f, Choice p) => Choice (PrismWrapper f p) where
  left' (PrismWrapper p) = PrismWrapper $ rmap sequenceL $ left' p
    where
      sequenceL (Left a) = fmap Left a
      sequenceL (Right a) = pure $ Right a

-- | Obtain a 'Prism' from a 'Profunctor'-based one.
toPrism :: (forall p. Choice p => p a b -> p s t) -> Prism s t a b
toPrism p = runPrismWrapper . p . PrismWrapper

-- | Obtain a 'Lens' from a 'Profunctor'-based one.
toLens :: (forall p. Strong p => p a b -> p s t) -> Lens s t a b
toLens p = runStar . p . Star

-- | Obtain a 'Setter' from a 'Profunctor'-based one.
toSetter :: (forall p. Mapping p => p a b -> p s t) -> Setter s t a b
toSetter p = runStar . p . Star

-- | Obtain a 'Traversal' from a 'Profunctor'-based one.
toTraversal :: (forall p. Traversing p => p a b -> p s t) -> Traversal s t a b
toTraversal p = runStar . p . Star

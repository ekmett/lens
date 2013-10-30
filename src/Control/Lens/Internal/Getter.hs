{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Getter
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Getter
  (
  -- * Internal Classes
    Gettable
  -- ** Getters
  , coerce
  , noEffect
  , AlongsideLeft(..)
  , AlongsideRight(..)
  ) where

import Control.Applicative
import Data.Functor.Contravariant
import Data.Foldable
import Data.Traversable
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Void

-- | This class is provided mostly for backwards compatibility with lens 3.8,
-- but it can also shorten type signatures.
class (Contravariant f, Functor f) => Gettable f
instance (Contravariant f, Functor f) => Gettable f

-------------------------------------------------------------------------------
-- Gettables & Accessors
-------------------------------------------------------------------------------

-- | This Generalizes 'Const' so we can apply simple 'Applicative'
-- transformations to it and so we can get nicer error messages.
--
-- A 'Functor' you can 'coerce' ignores its argument, which it carries solely as a
-- phantom type parameter.
--
-- By the 'Functor' and 'Contravariant' laws, an instance of 'Gettable' will necessarily satisfy:
--
-- @'id' = 'fmap' f = 'coerce' = 'contramap' g@
coerce :: (Contravariant f, Functor f) => f a -> f b
coerce a = absurd <$> contramap absurd a
{-# INLINE coerce #-}

-- | The 'mempty' equivalent for a 'Gettable' 'Applicative' 'Functor'.
noEffect :: (Contravariant f, Applicative f) => f a
noEffect = coerce $ pure ()
{-# INLINE noEffect #-}

newtype AlongsideLeft f b a = AlongsideLeft { getAlongsideLeft :: f (a, b) }

deriving instance Show (f (a, b)) => Show (AlongsideLeft f b a)
deriving instance Read (f (a, b)) => Read (AlongsideLeft f b a)

instance Functor f => Functor (AlongsideLeft f b) where
  fmap f = AlongsideLeft . fmap (first f) . getAlongsideLeft
  {-# INLINE fmap #-}

instance Contravariant f => Contravariant (AlongsideLeft f b) where
  contramap f = AlongsideLeft . contramap (first f) . getAlongsideLeft
  {-# INLINE contramap #-}

instance Foldable f => Foldable (AlongsideLeft f b) where
  foldMap f = foldMap (f . fst) . getAlongsideLeft
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (AlongsideLeft f b) where
  traverse f (AlongsideLeft as) = AlongsideLeft <$> traverse (bitraverse f pure) as
  {-# INLINE traverse #-}

instance Foldable1 f => Foldable1 (AlongsideLeft f b) where
  foldMap1 f = foldMap1 (f . fst) . getAlongsideLeft
  {-# INLINE foldMap1 #-}

instance Traversable1 f => Traversable1 (AlongsideLeft f b) where
  traverse1 f (AlongsideLeft as) = AlongsideLeft <$> traverse1 (\(a,b) -> flip (,) b <$> f a) as where
  {-# INLINE traverse1 #-}

instance Functor f => Bifunctor (AlongsideLeft f) where
  bimap f g = AlongsideLeft . fmap (bimap g f) . getAlongsideLeft
  {-# INLINE bimap #-}

instance Foldable f => Bifoldable (AlongsideLeft f) where
  bifoldMap f g = foldMap (bifoldMap g f) . getAlongsideLeft
  {-# INLINE bifoldMap #-}

instance Traversable f => Bitraversable (AlongsideLeft f) where
  bitraverse f g (AlongsideLeft as) = AlongsideLeft <$> traverse (bitraverse g f) as
  {-# INLINE bitraverse #-}

newtype AlongsideRight f a b = AlongsideRight { getAlongsideRight :: f (a, b) }

deriving instance Show (f (a, b)) => Show (AlongsideRight f a b)
deriving instance Read (f (a, b)) => Read (AlongsideRight f a b)

instance Functor f => Functor (AlongsideRight f a) where
  fmap f (AlongsideRight x) = AlongsideRight (fmap (second f) x)
  {-# INLINE fmap #-}

instance Contravariant f => Contravariant (AlongsideRight f a) where
  contramap f (AlongsideRight x) = AlongsideRight (contramap (second f) x)
  {-# INLINE contramap #-}

instance Foldable f => Foldable (AlongsideRight f a) where
  foldMap f = foldMap (f . snd) . getAlongsideRight
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (AlongsideRight f a) where
  traverse f (AlongsideRight as) = AlongsideRight <$> traverse (bitraverse pure f) as
  {-# INLINE traverse #-}

instance Foldable1 f => Foldable1 (AlongsideRight f a) where
  foldMap1 f = foldMap1 (f . snd) . getAlongsideRight
  {-# INLINE foldMap1 #-}

instance Traversable1 f => Traversable1 (AlongsideRight f a) where
  traverse1 f (AlongsideRight as) = AlongsideRight <$> traverse1 (\(a,b) -> (,) a <$> f b) as where
  {-# INLINE traverse1 #-}

instance Functor f => Bifunctor (AlongsideRight f) where
  bimap f g = AlongsideRight . fmap (bimap f g) . getAlongsideRight
  {-# INLINE bimap #-}

instance Foldable f => Bifoldable (AlongsideRight f) where
  bifoldMap f g = foldMap (bifoldMap f g) . getAlongsideRight
  {-# INLINE bifoldMap #-}

instance Traversable f => Bitraversable (AlongsideRight f) where
  bitraverse f g (AlongsideRight as) = AlongsideRight <$> traverse (bitraverse f g) as
  {-# INLINE bitraverse #-}

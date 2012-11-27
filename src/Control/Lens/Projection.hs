{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Projection
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-------------------------------------------------------------------------------
module Control.Lens.Projection
  (
  -- * Projections
    Projection
  -- * Constructing Projections
  , Projective(..)
  , Project(..)
  -- * Consuming Projections
  , Projecting
  , project
  , qua
  , review, reviews
  , reuse, reuses
  -- * Common projections
  , _left
  , _right
  -- * Simple
  , SimpleProjection
  ) where

import Control.Applicative
import Control.Category
import Control.Monad.Reader as Reader
import Control.Monad.State as State
import Control.Lens.Classes
import Control.Lens.Getter
import Control.Lens.Internal
import Control.Lens.Type
import Prelude hiding (id,(.))
import Unsafe.Coerce

-- $setup
-- >>> import Control.Lens

------------------------------------------------------------------------------
-- Projection Internals
------------------------------------------------------------------------------

-- | A 'Projection' is a 'Traversal' that can also be turned around with 'qua' to obtain a 'Getter'
type Projection s t a b = forall k f. (Projective k, Applicative f) => k (a -> f b) (s -> f t)

-- | A @'Simple' 'Projection'@.
type SimpleProjection s a = Projection s s a a

-- | Reflect a 'Projection'.
project :: (Projective k, Applicative f) => Overloaded Project f s t a b -> Overloaded k f s t a b
project (Project f g) = projecting (unsafeCoerce f) (unsafeCoerce g)

-- | Consume a 'Project'. This is commonly used when a function takes a 'Projection' as a parameter.
type Projecting f s t a b = Overloaded Project f s t a b

-- | Turn a 'Projection' around to get at its contents.
qua :: Projecting Mutator s t a b -> Getter b t
qua (Project bt _) = to (unsafeCoerce bt)

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Projection' around and 'view' a value (or the current environment) through it the other way.
--
-- @'review' ≡ 'view' '.' 'from'@
review :: MonadReader b m => Projecting Mutator s t a b -> m t
review (Project bt _) = asks (unsafeCoerce bt)
{-# INLINE review #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Projection' around and 'view' a value (or the current environment) through it the other way,
-- applying a function.
--
-- @'reviews' ≡ 'views' '.' 'from'@
reviews :: MonadReader b m => Projecting Mutator s t a b -> (t -> r) -> m r
reviews (Project bt _) f = asks (f . unsafeCoerce bt)
{-# INLINE reviews #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Projection' around and 'use' a value (or the current environment) through it the other way.
--
-- @'reuse' ≡ 'use' '.' 'from'@
reuse :: MonadState b m => Projecting Mutator s t a b -> m t
reuse (Project bt _) = gets (unsafeCoerce bt)
{-# INLINE reuse #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Projection' around and 'use' the current state through it the other way,
-- applying a function.
--
-- @'reuses' ≡ 'uses' '.' 'from'@
reuses :: MonadState b m => Projecting Mutator s t a b -> (t -> r) -> m r
reuses (Project bt _) f = gets (f . unsafeCoerce bt)
{-# INLINE reuses #-}

-- | A traversal for tweaking the left-hand value of an 'Either':
--
-- >>> over _left (+1) (Left 2)
-- Left 3
--
-- >>> over _left (+1) (Right 2)
-- Right 2
--
-- >>> Right 42 ^._left :: String
-- ""
--
-- >>> Left "hello" ^._left
-- "hello"
--
-- @'_left' :: 'Applicative' f => (a -> f b) -> 'Either' a c -> f ('Either' b c)@
_left :: Projection (Either a c) (Either b c) a b
_left = projecting Left $ \ f e -> case e of
  Left a  -> Left <$> f a
  Right c -> pure $ Right c
{-# INLINE _left #-}

-- | traverse the right-hand value of an 'Either':
--
-- @'_right' ≡ 'Data.Traversable.traverse'@
--
-- Unfortunately the instance for
-- @'Data.Traversable.Traversable' ('Either' c)@ is still missing from base,
-- so this can't just be 'Data.Traversable.traverse'
--
-- >>> over _right (+1) (Left 2)
-- Left 2
--
-- >>> over _right (+1) (Right 2)
-- Right 3
--
-- >>> Right "hello" ^._right
-- "hello"
--
-- >>> Left "hello" ^._right :: [Double]
-- []
--
-- @'_right' :: 'Applicative' f => (a -> f b) -> 'Either' c a -> f ('Either' c a)@
_right :: Projection (Either c a) (Either c b) a b
_right = projecting Right $ \f e -> case e of
  Left c -> pure $ Left c
  Right a -> Right <$> f a
{-# INLINE _right #-}

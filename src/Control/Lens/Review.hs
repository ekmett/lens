{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Review
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-------------------------------------------------------------------------------
module Control.Lens.Review
  (
  -- * Reviewing
    Review, Review'
  , AReview, AReview'
  , unto
  , remit
  , review, reviews
  , reuse, reuses
  -- * Reviewable Profunctors
  , Reviewable(..)
  ) where

import Control.Monad.Reader as Reader
import Control.Monad.State as State
import Control.Lens.Getter
import Control.Lens.Internal
import Control.Lens.Type
import Data.Functor.Identity
import Data.Profunctor
import Data.Profunctor.Unsafe

-- $setup
-- >>> import Control.Lens

------------------------------------------------------------------------------
-- Review
------------------------------------------------------------------------------

type Review s t a b = forall p f. (Reviewable p, Settable f) => Overloaded p f s t a b

type Review' t b = Review t t b b

-- | If you see this in a signature for a function, the function is expecting a 'Prism'
type AReview s t a b = Overloaded Reviewed Identity s t a b

type AReview' t b = AReview t t b b

-- | An analogue of 'to' for 'review'.
--
-- @'unto' :: (b -> t) -> 'Review'' t b@
unto :: (Reviewable p, Functor f) => (b -> t) -> Overloaded p f s t a b
unto f = retagged . rmap (fmap f)

-- | Turn a 'Prism' or 'Control.Lens.Iso.Iso' around to build a 'Getter'.
--
-- If you have an 'Control.Lens.Iso.Iso', 'Control.Lens.Iso.from' is a more powerful version of this function
-- that will return an 'Control.Lens.Iso.Iso' instead of a mere 'Getter'.
--
-- >>> 5 ^.remit _left
-- Left 5
--
-- @
-- 'remit' :: 'Prism' s t a b -> 'Getter' b t
-- 'remit' :: 'Iso' s t a b   -> 'Getter' b t
-- @
remit :: AReview s t a b -> Getter b t
remit p = to (runIdentity #. runReviewed #. p .# Reviewed .# Identity)
{-# INLINE remit #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Prism' around and 'view' a value (or the current environment) through it the other way.
--
-- @'review' ≡ 'view' '.' 'remit'@
--
-- >>> review _left "mustard"
-- Left "mustard"
--
-- Usually 'review' is used in the @(->)@ monad with a 'Prism'' or 'Control.Lens.Iso.Iso', in which case it may be useful to think of
-- it as having one of these more restricted type signatures:
--
-- @
-- 'review' :: 'Iso'' s a        -> a -> s
-- 'review' :: 'Prism'' s a -> a -> s
-- @
--
-- However, when working with a monad transformer stack, it is sometimes useful to be able to 'review' the current environment, in which case one of
-- these more slightly more liberal type signatures may be beneficial to think of it as having:
--
-- @
-- 'review' :: 'MonadReader' a m => 'Iso'' s a        -> m s
-- 'review' :: 'MonadReader' a m => 'Prism'' s a -> m s
-- @
review :: MonadReader b m => AReview s t a b -> m t
review p = asks (runIdentity #. runReviewed #. p .# Reviewed .# Identity)
{-# INLINE review #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Prism' around and 'view' a value (or the current environment) through it the other way,
-- applying a function.
--
-- @'reviews' ≡ 'views' '.' 'remit'@
--
-- >>> reviews _left isRight "mustard"
-- False
--
-- Usually this function is used in the @(->)@ monad with a 'Prism'' or 'Control.Lens.Iso.Iso', in which case it may be useful to think of
-- it as having one of these more restricted type signatures:
--
-- @
-- 'reviews' :: 'Iso'' s a        -> (s -> r) -> a -> r
-- 'reviews' :: 'Prism'' s a -> (s -> r) -> a -> r
-- @
--
-- However, when working with a monad transformer stack, it is sometimes useful to be able to 'review' the current environment, in which case one of
-- these more slightly more liberal type signatures may be beneficial to think of it as having:
--
-- @
-- 'reviews' :: 'MonadReader' a m => 'Iso'' s a   -> (s -> r) -> m r
-- 'reviews' :: 'MonadReader' a m => 'Prism'' s a -> (s -> r) -> m r
-- @
reviews :: MonadReader b m => AReview s t a b -> (t -> r) -> m r
reviews p tr = asks (tr . runIdentity #. runReviewed #. p .# Reviewed .# Identity)
{-# INLINE reviews #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Prism' around and 'use' a value (or the current environment) through it the other way.
--
-- @'reuse' ≡ 'use' '.' 'remit'@
--
-- >>> evalState (reuse _left) 5
-- Left 5
--
-- @
-- 'reuse' :: 'MonadState' a m => 'Prism'' s a -> m s
-- 'reuse' :: 'MonadState' a m => 'Iso'' s a   -> m s
-- @
reuse :: MonadState b m => AReview s t a b -> m t
reuse p = gets (runIdentity #. runReviewed #. p .# Reviewed .# Identity)
{-# INLINE reuse #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Prism' around and 'use' the current state through it the other way,
-- applying a function.
--
-- @'reuses' ≡ 'uses' '.' 'remit'@
--
-- >>> evalState (reuses _left isLeft) (5 :: Int)
-- True
--
-- @
-- 'reuses' :: 'MonadState' a m => 'Prism'' s a -> (s -> r) -> m r
-- 'reuses' :: 'MonadState' a m => 'Iso'' s a   -> (s -> r) -> m r
-- @
reuses :: MonadState b m => AReview s t a b -> (t -> r) -> m r
reuses p tr = gets (tr . runIdentity #. runReviewed #. p .# Reviewed .# Identity)
{-# INLINE reuses #-}

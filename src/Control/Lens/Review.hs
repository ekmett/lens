{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-trustworthy-safe #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Review
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- A 'Review' is a type-restricted form of a 'Prism' that can only be used for
-- writing back via 're', 'review', 'reuse'.
-------------------------------------------------------------------------------
module Control.Lens.Review
  (
  -- * Reviewing
    Review
  , AReview
  , unto
  , un
  , re
  , review, reviews
  , reuse, reuses
  , (#)
  , Bifunctor(bimap)
  , retagged
  , Reviewable
  , reviewing
  ) where

import Control.Monad.Reader as Reader
import Control.Monad.State as State
import Control.Lens.Getter
import Control.Lens.Internal.Review
import Control.Lens.Type
import Data.Bifunctor
import Data.Functor.Identity
import Data.Profunctor
import Data.Profunctor.Unsafe
import Data.Tagged
import Data.Void

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import Control.Monad.State
-- >>> import Numeric.Lens
-- >>> import Data.Semigroup (Semigroup (..))
-- >>> let isLeft  (Left  _) = True; isLeft  _ = False
-- >>> let isRight (Right _) = True; isRight _ = False

infixr 8 #

------------------------------------------------------------------------------
-- Review
------------------------------------------------------------------------------

-- | An analogue of 'to' for 'review'.
--
-- @
-- 'unto' :: (b -> t) -> 'Review'' t b
-- @
--
-- @
-- 'unto' = 'un' . 'to'
-- @
unto :: (Profunctor p, Bifunctor p, Functor f) => (b -> t) -> Optic p f s t a b
unto f = first absurd . lmap absurd . rmap (fmap f)
{-# INLINE unto #-}

-- | Turn a 'Getter' around to get a 'Review'
--
-- @
-- 'un' = 'unto' . 'view'
-- 'unto' = 'un' . 'to'
-- @
--
-- >>> un (to length) # [1,2,3]
-- 3
un :: (Profunctor p, Bifunctor p, Functor f) => Getting a s a -> Optic' p f a s
un = unto . view

-- | Turn a 'Prism' or 'Control.Lens.Iso.Iso' around to build a 'Getter'.
--
-- If you have an 'Control.Lens.Iso.Iso', 'Control.Lens.Iso.from' is a more powerful version of this function
-- that will return an 'Control.Lens.Iso.Iso' instead of a mere 'Getter'.
--
-- >>> 5 ^.re _Left
-- Left 5
--
-- >>> 6 ^.re (_Left.unto succ)
-- Left 7
--
-- @
-- 'review'  ≡ 'view'  '.' 're'
-- 'reviews' ≡ 'views' '.' 're'
-- 'reuse'   ≡ 'use'   '.' 're'
-- 'reuses'  ≡ 'uses'  '.' 're'
-- @
--
-- @
-- 're' :: 'Prism' s t a b -> 'Getter' b t
-- 're' :: 'Iso' s t a b   -> 'Getter' b t
-- @
re :: AReview t b -> Getter b t
re p = to (runIdentity #. unTagged #. p .# Tagged .# Identity)
{-# INLINE re #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Prism' around and 'view' a value (or the current environment) through it the other way.
--
-- @
-- 'review' ≡ 'view' '.' 're'
-- 'review' . 'unto' ≡ 'id'
-- @
--
-- >>> review _Left "mustard"
-- Left "mustard"
--
-- >>> review (unto succ) 5
-- 6
--
-- Usually 'review' is used in the @(->)@ 'Monad' with a 'Prism' or 'Control.Lens.Iso.Iso', in which case it may be useful to think of
-- it as having one of these more restricted type signatures:
--
-- @
-- 'review' :: 'Iso'' s a   -> a -> s
-- 'review' :: 'Prism'' s a -> a -> s
-- @
--
-- However, when working with a 'Monad' transformer stack, it is sometimes useful to be able to 'review' the current environment, in which case
-- it may be beneficial to think of it as having one of these slightly more liberal type signatures:
--
-- @
-- 'review' :: 'MonadReader' a m => 'Iso'' s a   -> m s
-- 'review' :: 'MonadReader' a m => 'Prism'' s a -> m s
-- @
review :: MonadReader b m => AReview t b -> m t
review p = asks (runIdentity #. unTagged #. p .# Tagged .# Identity)
{-# INLINE review #-}

-- | An infix alias for 'review'.
--
-- @
-- 'unto' f # x ≡ f x
-- l # x ≡ x '^.' 're' l
-- @
--
-- This is commonly used when using a 'Prism' as a smart constructor.
--
-- >>> _Left # 4
-- Left 4
--
-- But it can be used for any 'Prism'
--
-- >>> base 16 # 123
-- "7b"
--
-- @
-- (#) :: 'Iso''      s a -> a -> s
-- (#) :: 'Prism''    s a -> a -> s
-- (#) :: 'Review'    s a -> a -> s
-- (#) :: 'Equality'' s a -> a -> s
-- @
(#) :: AReview t b -> b -> t
(#) p = runIdentity #. unTagged #. p .# Tagged .# Identity
{-# INLINE (#) #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Prism' around and 'view' a value (or the current environment) through it the other way,
-- applying a function.
--
-- @
-- 'reviews' ≡ 'views' '.' 're'
-- 'reviews' ('unto' f) g ≡ g '.' f
-- @
--
-- >>> reviews _Left isRight "mustard"
-- False
--
-- >>> reviews (unto succ) (*2) 3
-- 8
--
-- Usually this function is used in the @(->)@ 'Monad' with a 'Prism' or 'Control.Lens.Iso.Iso', in which case it may be useful to think of
-- it as having one of these more restricted type signatures:
--
-- @
-- 'reviews' :: 'Iso'' s a   -> (s -> r) -> a -> r
-- 'reviews' :: 'Prism'' s a -> (s -> r) -> a -> r
-- @
--
-- However, when working with a 'Monad' transformer stack, it is sometimes useful to be able to 'review' the current environment, in which case
-- it may be beneficial to think of it as having one of these slightly more liberal type signatures:
--
-- @
-- 'reviews' :: 'MonadReader' a m => 'Iso'' s a   -> (s -> r) -> m r
-- 'reviews' :: 'MonadReader' a m => 'Prism'' s a -> (s -> r) -> m r
-- @
reviews :: MonadReader b m => AReview t b -> (t -> r) -> m r
reviews p tr = asks (tr . runIdentity #. unTagged #. p .# Tagged .# Identity)
{-# INLINE reviews #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Prism' around and 'use' a value (or the current environment) through it the other way.
--
-- @
-- 'reuse' ≡ 'use' '.' 're'
-- 'reuse' '.' 'unto' ≡ 'gets'
-- @
--
-- >>> evalState (reuse _Left) 5
-- Left 5
--
-- >>> evalState (reuse (unto succ)) 5
-- 6
--
-- @
-- 'reuse' :: 'MonadState' a m => 'Prism'' s a -> m s
-- 'reuse' :: 'MonadState' a m => 'Iso'' s a   -> m s
-- @
reuse :: MonadState b m => AReview t b -> m t
reuse p = gets (runIdentity #. unTagged #. p .# Tagged .# Identity)
{-# INLINE reuse #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Prism' around and 'use' the current state through it the other way,
-- applying a function.
--
-- @
-- 'reuses' ≡ 'uses' '.' 're'
-- 'reuses' ('unto' f) g ≡ 'gets' (g '.' f)
-- @
--
-- >>> evalState (reuses _Left isLeft) (5 :: Int)
-- True
--
-- @
-- 'reuses' :: 'MonadState' a m => 'Prism'' s a -> (s -> r) -> m r
-- 'reuses' :: 'MonadState' a m => 'Iso'' s a   -> (s -> r) -> m r
-- @
reuses :: MonadState b m => AReview t b -> (t -> r) -> m r
reuses p tr = gets (tr . runIdentity #. unTagged #. p .# Tagged .# Identity)
{-# INLINE reuses #-}

-- | Coerce a polymorphic 'Prism' to a 'Review'.
--
-- @
-- 'reviewing' :: 'Iso' s t a b -> 'Review' t b
-- 'reviewing' :: 'Prism' s t a b -> 'Review' t b
-- @
reviewing :: (Bifunctor p, Functor f) => Optic Tagged Identity s t a b -> Optic' p f t b
reviewing p = bimap f (fmap f) where
  f = runIdentity . unTagged . p . Tagged . Identity

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
  , remit
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
-- >>> import Numeric.Natural
-- >>> let nat :: Simple Projection Integer Natural; nat = projecting toInteger $ \f i -> if i < 0 then pure i else toInteger <$> f (fromInteger i)

------------------------------------------------------------------------------
-- Projection Internals
------------------------------------------------------------------------------

-- | A 'Projection' @l@ is a 0-or-1 target 'Control.Lens.Traversal.Traversal' that can also be turned around with 'remit' to
-- obtain a 'Getter' in the opposite direction, such that in addition to the 'Control.Lens.Traversal.Traversal' laws, we also
-- have
--
-- @x '^.' 'remit' l '^?' l ≡ 'Just' x@
--
-- @'Control.Lens.Fold.lengthOf' l x '<=' 1@
--
-- Every 'Projection' is a valid 'Control.Lens.Traversal.Traversal'.
--
-- Every 'Control.Lens.Iso.Iso' is a valid 'Projection'.
--
-- It may help to think of this as a 'Control.Lens.Iso.Iso' that is partial in one direction.
--
-- For example, you might have a @'Simple' 'Projection' 'Integer' Natural@ allows you to always
-- go from a 'Natural' to an 'Integer', and provide you with tools to check if an 'Integer' is
-- a 'Natural' and/or to edit one if it is.
--
-- @
-- 'nat' :: 'Simple' 'Projection' 'Integer' 'Numeric.Natural.Natural'
-- 'nat' = 'projecting' 'toInteger' '$' \\ f i ->
--    if i '<' 0
--    then 'pure' i
--    else 'toInteger' '<$>' f ('fromInteger' i)
-- @
--
-- Now we can ask if an 'Integer' is a 'Natural'.
--
-- >>> 5^?nat
-- Just 5
--
-- >>> (-5)^?nat
-- Nothing
--
-- We can update the ones that are:
--
-- >>> (-3,4) & both.nat *~ 2
-- (-3,8)
--
-- And we can then convert from a 'Natural' to an 'Integer'.
--
-- >>> 5 ^. remit nat
-- 5
--
-- Similarly we can use a 'Projection' to 'Control.Lens.Traversal.traverse' the left half of an 'Either':
--
-- >>> Left "hello" & _left %~ length
-- Left 5
--
-- or to construct an 'Either':
--
-- >>> 5^.remit _left
-- Left 5
--
-- such that if you query it with the 'Projection', you will get your original input back.
--
-- >>> 5^.remit _left ^? _left
-- Just 5
type Projection s t a b = forall k f. (Projective k, Applicative f) => k (a -> f b) (s -> f t)

-- | A @'Simple' 'Projection'@.
type SimpleProjection s a = Projection s s a a

-- | Reflect a 'Projection'.
project :: (Projective k, Applicative f) => Overloaded Project f s t a b -> Overloaded k f s t a b
project (Project f g) = projecting (unsafeCoerce f) (unsafeCoerce g)

-- | Consume a 'Project'. This is commonly used when a function takes a 'Projection' as a parameter.
type Projecting f s t a b = Overloaded Project f s t a b

-- | Turn a 'Projection' (or 'Control.Lens.Iso.Iso') around to get at its contents.
--
-- >>> 5 ^.remit _left
-- Left 5
remit :: Projecting Mutator s t a b -> Getter b t
remit (Project bt _) = to (unsafeCoerce bt)

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Projection' around and 'view' a value (or the current environment) through it the other way.
--
-- @'review' ≡ 'view' '.' 'remit'@
review :: MonadReader b m => Projecting Mutator s t a b -> m t
review (Project bt _) = asks (unsafeCoerce bt)
{-# INLINE review #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Projection' around and 'view' a value (or the current environment) through it the other way,
-- applying a function.
--
-- @'reviews' ≡ 'views' '.' 'remit'@
reviews :: MonadReader b m => Projecting Mutator s t a b -> (t -> r) -> m r
reviews (Project bt _) f = asks (f . unsafeCoerce bt)
{-# INLINE reviews #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Projection' around and 'use' a value (or the current environment) through it the other way.
--
-- @'reuse' ≡ 'use' '.' 'remit'@
reuse :: MonadState b m => Projecting Mutator s t a b -> m t
reuse (Project bt _) = gets (unsafeCoerce bt)
{-# INLINE reuse #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Projection' around and 'use' the current state through it the other way,
-- applying a function.
--
-- @'reuses' ≡ 'uses' '.' 'remit'@
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

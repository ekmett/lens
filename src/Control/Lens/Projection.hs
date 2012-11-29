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
  , projected

  -- * Consuming Projections
  , Projecting
  , cloneProjection
  , remit
  , review, reviews
  , reuse, reuses

  -- * Common projections
  , _left
  , _right

  -- * Projective Lenses
  , ProjectiveLens

  -- * Consuming projective lenses
  , cloneProjectiveLens

  -- * Simple
  , SimpleProjection
  , SimpleProjectiveLens
  ) where

import Control.Applicative
import Control.Category
import Control.Monad.Reader as Reader
import Control.Monad.State as State
import Control.Lens.Classes
import Control.Lens.Getter
import Control.Lens.Internal
import Control.Lens.Traversal
import Control.Lens.Type
import Prelude hiding (id,(.))
import Unsafe.Coerce

-- $setup
-- >>> import Control.Lens
-- >>> import Numeric.Natural
-- >>> let nat :: Simple Projection Integer Natural; nat = projecting toInteger $ \f i -> if i < 0 then pure i else toInteger <$> f (fromInteger i)
-- >>> let isLeft  (Left  _) = True; isLeft  _ = False
-- >>> let isRight (Right _) = True; isRight _ = False

------------------------------------------------------------------------------
-- Projection Internals
------------------------------------------------------------------------------

-- | A 'Projection' @l@ is a 0-or-1 target 'Traversal' that can also be turned around with 'remit' to
-- obtain a 'Getter' in the opposite direction, such that in addition to the 'Traversal' laws, we also
-- have
--
-- @x '^.' 'remit' l '^?' l ≡ 'Just' x@
--
-- @'Control.Lens.Fold.lengthOf' l x '<=' 1@
--
-- Every 'Projection' is a valid 'Traversal'.
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
-- Similarly we can use a 'Projection' to 'traverse' the left half of an 'Either':
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

-- | A 'ProjectiveLens' @l@ is a 'Lens' that can also be turned around with 'remit' to
-- obtain a 'Getter' in the opposite direction, such that in addition to the 'Lens' laws, we also
-- satisfy the 'Projection' laws.
type ProjectiveLens s t a b = forall k f. (Projective k, Functor f) => k (a -> f b) (s -> f t)

-- | A @'Simple' 'Projection'@.
type SimpleProjection s a = Projection s s a a

-- | @'Simple' 'ProjectiveLens'@
type SimpleProjectiveLens s a = ProjectiveLens s s a a

-- | Consume a 'Project'. This is commonly used when a function takes a 'Projection' as a parameter.
type Projecting f s t a b = Overloaded Project f s t a b

-- | Construct a 'Projection' from a projection/embedding pair.
--
-- @'Either' t a@ is used instead of @'Maybe' a@ to permit the types of @s@ and @t@ to differ.
projected :: (b -> t) -> (s -> Either t a) -> Projection s t a b
projected bt sma = projecting bt $ \f s -> case sma s of
  Left  t -> pure t
  Right a -> bt <$> f a

-- | Clone a 'Projection' so that you can reuse the same monomorphically typed 'Projection' for different purposes.
--
-- See 'cloneLens' and 'cloneTraversal' for examples of why you might want to do this.
cloneProjection :: Projecting (Bazaar a b) s t a b -> Projection s t a b
cloneProjection (Project f g) = projecting (unsafeCoerce f) (cloneTraversal (unsafeCoerce g))

-- | Clone a 'ProjectiveLens'. Similar to 'cloneLens', 'cloneProjection', 'cloneTraversal' this
-- can permit you to reuse a projective lens several times in a function for different purposes
-- without a rank-2 type and explicit signature.
cloneProjectiveLens :: Projecting (Context a b) s t a b -> ProjectiveLens s t a b
cloneProjectiveLens (Project f g) = projecting (unsafeCoerce f) (cloneLens (unsafeCoerce g))

------------------------------------------------------------------------------
-- Projection Combinators
------------------------------------------------------------------------------

-- | Turn a 'Projection' or 'Control.Lens.Iso.Iso' around to build a 'Getter'.
--
-- If you have an 'Control.Lens.Iso.Iso', 'Control.Lens.Iso.from' is a more powerful version of this function
-- that will return an 'Control.Lens.Iso.Iso' instead of a mere 'Getter'.
--
-- >>> 5 ^.remit _left
-- Left 5
--
-- @
-- 'remit' :: 'Projection' s t a b -> 'Getter' b t
-- 'remit' :: 'Iso' s t a b        -> 'Getter' b t
-- @
remit :: Projecting Mutator s t a b -> Getter b t
remit (Project bt _) = to (unsafeCoerce bt)

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Projection' around and 'view' a value (or the current environment) through it the other way.
--
-- @'review' ≡ 'view' '.' 'remit'@
--
-- >>> review _left "mustard"
-- Left "mustard"
--
-- Usually 'review' is used in the @(->)@ monad with a 'Simple' 'Projection' or 'Control.Lens.Iso.Iso', in which case it may be useful to think of
-- it as having one of these more restricted type signatures:
--
-- @
-- 'review' :: 'Simple' 'Iso' s a        -> a -> s
-- 'review' :: 'Simple' 'Projection' s a -> a -> s
-- @
--
-- However, when working with a monad transformer stack, it is sometimes useful to be able to 'review' the current environment, in which case one of 
-- these more slightly more liberal type signatures may be beneficial to think of it as having:
--
-- @
-- 'review' :: 'MonadReader' a m => 'Simple' 'Iso' s a        -> m s
-- 'review' :: 'MonadReader' a m => 'Simple' 'Projection' s a -> m s
-- @
review :: MonadReader b m => Projecting Mutator s t a b -> m t
review (Project bt _) = asks (unsafeCoerce bt)
{-# INLINE review #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Projection' around and 'view' a value (or the current environment) through it the other way,
-- applying a function.
--
-- @'reviews' ≡ 'views' '.' 'remit'@
--
-- >>> reviews _left isRight "mustard"
-- False
--
-- Usually this function is used in the @(->)@ monad with a 'Simple' 'Projection' or 'Control.Lens.Iso.Iso', in which case it may be useful to think of
-- it as having one of these more restricted type signatures:
--
-- @
-- 'reviews' :: 'Simple' 'Iso' s a        -> (s -> r) -> a -> r
-- 'reviews' :: 'Simple' 'Projection' s a -> (s -> r) -> a -> r
-- @
--
-- However, when working with a monad transformer stack, it is sometimes useful to be able to 'review' the current environment, in which case one of 
-- these more slightly more liberal type signatures may be beneficial to think of it as having:
--
-- @
-- 'reviews' :: 'MonadReader' a m => 'Simple' 'Iso' s a        -> (s -> r) -> m r
-- 'reviews' :: 'MonadReader' a m => 'Simple' 'Projection' s a -> (s -> r) -> m r
-- @
reviews :: MonadReader b m => Projecting Mutator s t a b -> (t -> r) -> m r
reviews (Project bt _) f = asks (f . unsafeCoerce bt)
{-# INLINE reviews #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Projection' around and 'use' a value (or the current environment) through it the other way.
--
-- @'reuse' ≡ 'use' '.' 'remit'@
--
-- >>> evalState (reuse _left) 5
-- Left 5
--
-- @
-- 'reuse' :: 'MonadState' a m => 'Simple' 'Projection' s a -> m s
-- 'reuse' :: 'MonadState' a m => 'Simple' 'Iso' s a        -> m s
-- @
reuse :: MonadState b m => Projecting Mutator s t a b -> m t
reuse (Project bt _) = gets (unsafeCoerce bt)
{-# INLINE reuse #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Projection' around and 'use' the current state through it the other way,
-- applying a function.
--
-- @'reuses' ≡ 'uses' '.' 'remit'@
--
-- >>> evalState (reuses _left isLeft) 5
-- True
--
-- @
-- 'reuses' :: 'MonadState' a m => 'Simple' 'Projection' s a -> (s -> r) -> m r
-- 'reuses' :: 'MonadState' a m => 'Simple' 'Iso' s a        -> (s -> r) -> m r
-- @
reuses :: MonadState b m => Projecting Mutator s t a b -> (t -> r) -> m r
reuses (Project bt _) f = gets (f . unsafeCoerce bt)
{-# INLINE reuses #-}

------------------------------------------------------------------------------
-- Common Projections
------------------------------------------------------------------------------

-- | This projection provides a traversal for tweaking the left-hand value of an 'Either':
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
-- It also can be turned around to obtain the embedding into the 'Left' half of an 'Either'
--
-- >>> 5^.remit _left
-- Left 5
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
-- It also can be turned around to obtain the embedding into the 'Left' half of an 'Either'
--
-- >>> 5^.remit _right
-- Right 5
_right :: Projection (Either c a) (Either c b) a b
_right = projecting Right $ \f e -> case e of
  Left c -> pure $ Left c
  Right a -> Right <$> f a
{-# INLINE _right #-}

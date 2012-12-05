{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Prism
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-------------------------------------------------------------------------------
module Control.Lens.Prism
  (
  -- * Prisms
    Prism

  -- * Constructing Prisms
  , Prismatic(..)

  -- * Consuming Prisms
  , clonePrism
  , remit
  , review, reviews
  , reuse, reuses
  , outside
  , aside
  , without

  -- * Common projections
  , _left
  , _right

  -- * Simple
  , SimplePrism
  ) where

import Control.Arrow
import Control.Category
import Control.Monad.Reader as Reader
import Control.Monad.State as State
import Control.Lens.Classes
import Control.Lens.Combinators
import Control.Lens.Getter
import Control.Lens.Internal
import Control.Lens.Type
import Prelude hiding (id,(.))

-- $setup
-- >>> import Control.Lens
-- >>> import Numeric.Natural
-- >>> :set -XFlexibleContexts -XTypeFamilies
-- >>> let nat :: Simple Prism Integer Natural; nat = prism toInteger $ \i -> if i <= 0 then Left i else Right (fromInteger i)
-- >>> let isLeft  (Left  _) = True; isLeft  _ = False
-- >>> let isRight (Right _) = True; isRight _ = False

------------------------------------------------------------------------------
-- Prism Internals
------------------------------------------------------------------------------

-- | A 'Prism' @l@ is a 0-or-1 target 'Traversal' that can also be turned around with 'remit' to
-- obtain a 'Getter' in the opposite direction, such that in addition to the 'Traversal' laws, we also
-- have
--
-- @x '^.' 'remit' l '^?' l ≡ 'Just' x@
--
-- @'Control.Lens.Fold.lengthOf' l x '<=' 1@
--
-- It may help to think of this as a 'Control.Lens.Iso.Iso' that can be partial in one direction.
--
-- Every 'Prism' is a valid 'Traversal'.
--
-- Every 'Control.Lens.Iso.Iso' is a valid 'Prism'.
--
-- For example, you might have a @'Simple' 'Prism' 'Integer' Natural@ allows you to always
-- go from a 'Natural' to an 'Integer', and provide you with tools to check if an 'Integer' is
-- a 'Natural' and/or to edit one if it is.
--
--
-- @
-- 'nat' :: 'Simple' 'Prism' 'Integer' 'Numeric.Natural.Natural'
-- 'nat' = 'prism' 'toInteger' '$' \\ i ->
--    if i '<' 0
--    then 'Left' i
--    else 'Right' ('fromInteger' i)
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
-- >>> 5 ^. remit nat -- :: Natural
-- 5
--
-- Similarly we can use a 'Prism' to 'traverse' the left half of an 'Either':
--
-- >>> Left "hello" & _left %~ length
-- Left 5
--
-- or to construct an 'Either':
--
-- >>> 5^.remit _left
-- Left 5
--
-- such that if you query it with the 'Prism', you will get your original input back.
--
-- >>> 5^.remit _left ^? _left
-- Just 5
--
-- Another interesting way to think of a 'Prism' is as the categorical dual of a 'Lens'
-- a /co/-'Lens', so to speak. This is what permits the construction of 'outside'.
type Prism s t a b = forall r. (Prismatic r, S r ~ s, T r ~ t, A r ~ a, B r ~ b) => r

-- | A @'Simple' 'Prism'@.
type SimplePrism s a = Prism s s a a

-- | Clone a 'Prism' so that you can reuse the same monomorphically typed 'Prism' for different purposes.
--
-- See 'cloneLens' and 'cloneTraversal' for examples of why you might want to do this.
clonePrism :: APrism s t a b -> Prism s t a b
clonePrism (Prism f g) = prism f g

------------------------------------------------------------------------------
-- Prism Combinators
------------------------------------------------------------------------------

-- | Use a 'Prism' as a kind of first-class pattern.
--
-- @'outside' :: 'Prism' s t a b -> 'Lens' (t -> r) (s -> r) (b -> r) (a -> r)@
outside :: APrism s t a b -> Lens (t -> r) (s -> r) (b -> r) (a -> r)
outside (Prism bt seta) f tr = f (tr.bt) <&> \ar -> either tr ar . seta

-- | Use a 'Prism' to work over part of a structure.
aside :: APrism s t a b -> Prism (e, s) (e, t) (e, a) (e, b)
aside (Prism bt seta) = prism (fmap bt) $ \(e,s) -> case seta s of
  Left t -> Left (e,t)
  Right a -> Right (e,a)

-- | Given a pair of projections, project sums.
--
-- Viewing a 'Prism' as a co-lens, this combinator can be seen to be dual to 'alongside'.
without :: APrism s t a b
        -> APrism u v c d
        -> Prism (Either s u) (Either t v) (Either a c) (Either b d)
without (Prism bt seta) (Prism dv uevc) = prism (bt +++ dv) go where
  go (Left s) = either (Left . Left) (Right . Left) (seta s)
  go (Right u) = either (Left . Right) (Right . Right) (uevc u)

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
-- 'remit' :: 'Iso' s t a b        -> 'Getter' b t
-- @
remit :: APrism s t a b -> Getter b t
remit (Prism bt _) = to bt

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Prism' around and 'view' a value (or the current environment) through it the other way.
--
-- @'review' ≡ 'view' '.' 'remit'@
--
-- >>> review _left "mustard"
-- Left "mustard"
--
-- Usually 'review' is used in the @(->)@ monad with a 'Simple' 'Prism' or 'Control.Lens.Iso.Iso', in which case it may be useful to think of
-- it as having one of these more restricted type signatures:
--
-- @
-- 'review' :: 'Simple' 'Iso' s a        -> a -> s
-- 'review' :: 'Simple' 'Prism' s a -> a -> s
-- @
--
-- However, when working with a monad transformer stack, it is sometimes useful to be able to 'review' the current environment, in which case one of 
-- these more slightly more liberal type signatures may be beneficial to think of it as having:
--
-- @
-- 'review' :: 'MonadReader' a m => 'Simple' 'Iso' s a        -> m s
-- 'review' :: 'MonadReader' a m => 'Simple' 'Prism' s a -> m s
-- @
review :: MonadReader b m => APrism s t a b -> m t
review (Prism bt _) = asks bt
{-# INLINE review #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Prism' around and 'view' a value (or the current environment) through it the other way,
-- applying a function.
--
-- @'reviews' ≡ 'views' '.' 'remit'@
--
-- >>> reviews _left isRight "mustard"
-- False
--
-- Usually this function is used in the @(->)@ monad with a 'Simple' 'Prism' or 'Control.Lens.Iso.Iso', in which case it may be useful to think of
-- it as having one of these more restricted type signatures:
--
-- @
-- 'reviews' :: 'Simple' 'Iso' s a        -> (s -> r) -> a -> r
-- 'reviews' :: 'Simple' 'Prism' s a -> (s -> r) -> a -> r
-- @
--
-- However, when working with a monad transformer stack, it is sometimes useful to be able to 'review' the current environment, in which case one of 
-- these more slightly more liberal type signatures may be beneficial to think of it as having:
--
-- @
-- 'reviews' :: 'MonadReader' a m => 'Simple' 'Iso' s a        -> (s -> r) -> m r
-- 'reviews' :: 'MonadReader' a m => 'Simple' 'Prism' s a -> (s -> r) -> m r
-- @
reviews :: MonadReader b m => APrism s t a b -> (t -> r) -> m r
reviews (Prism bt _) f = asks (f . bt)
{-# INLINE reviews #-}

-- | This can be used to turn an 'Control.Lens.Iso.Iso' or 'Prism' around and 'use' a value (or the current environment) through it the other way.
--
-- @'reuse' ≡ 'use' '.' 'remit'@
--
-- >>> evalState (reuse _left) 5
-- Left 5
--
-- @
-- 'reuse' :: 'MonadState' a m => 'Simple' 'Prism' s a -> m s
-- 'reuse' :: 'MonadState' a m => 'Simple' 'Iso' s a        -> m s
-- @
reuse :: MonadState b m => APrism s t a b -> m t
reuse (Prism bt _) = gets bt
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
-- 'reuses' :: 'MonadState' a m => 'Simple' 'Prism' s a -> (s -> r) -> m r
-- 'reuses' :: 'MonadState' a m => 'Simple' 'Iso' s a        -> (s -> r) -> m r
-- @
reuses :: MonadState b m => APrism s t a b -> (t -> r) -> m r
reuses (Prism bt _) f = gets (f . bt)
{-# INLINE reuses #-}

------------------------------------------------------------------------------
-- Common Prisms
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
_left :: Prism (Either a c) (Either b c) a b
_left = prism Left $ either Right (Left . Right)
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
_right :: Prism (Either c a) (Either c b) a b
_right = prism Right (left Left)
{-# INLINE _right #-}

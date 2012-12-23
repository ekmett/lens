{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
#ifdef TRUSTWORTHY
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
    Prism, Prism'
  , APrism, APrism'
  , Reviewing, Reviewing'
  -- * Constructing Prisms
  , prism
  , prism'
  -- * Consuming Prisms
  , clonePrism
  , runPrism
  , remit
  , review, reviews
  , reuse, reuses
  , outside
  , aside
  , without
  -- * Common Prisms
  , _left
  , _right
  , _just
  -- * Prismatic profunctors
  , Prismatic(..)
  ) where

import Control.Applicative
import Control.Monad.Reader as Reader
import Control.Monad.State as State
import Control.Lens.Combinators
import Control.Lens.Getter
import Control.Lens.Internal
import Control.Lens.Type
import Data.Bifunctor
import Data.Functor.Identity
import Data.Profunctor
#ifndef SAFE
import Unsafe.Coerce
#endif

-- $setup
-- >>> import Control.Lens
-- >>> import Numeric.Natural
-- >>> let isLeft  (Left  _) = True; isLeft  _ = False
-- >>> let isRight (Right _) = True; isRight _ = False

------------------------------------------------------------------------------
-- Prism Internals
------------------------------------------------------------------------------

-- | If you see this in a signature for a function, the function is expecting a 'Prism'
type Reviewing s t a b = Overloading Review Review Identity s t a b

type Reviewing'  s a = Reviewing s s a a

-- | If you see this in a signature for a function, the function is expecting a 'Prism'.
type APrism s t a b = Market a b a (Mutator b) -> Market a b s (Mutator t)

type APrism' s a = APrism s s a a

-- | Safely decompose 'APrism'
runPrism :: APrism s t a b -> (b -> t, s -> Either t a)
#ifdef SAFE
runPrism k = case runMarket (k (Market (Mutator, Right))) of
  (bt, sa) -> (runMutator #. bt,  either (Left . runMutator) Right . sa)
#else
runPrism k = unsafeCoerce (runMarket (k (Market (Mutator, Right))))
#endif
{-# INLINE runPrism #-}

-- | Clone a 'Prism' so that you can reuse the same monomorphically typed 'Prism' for different purposes.
--
-- See 'cloneLens' and 'cloneTraversal' for examples of why you might want to do this.
clonePrism :: APrism s t a b -> Prism s t a b
clonePrism k = case runPrism k of
  (bt, sa) -> prism bt sa
{-# INLINE clonePrism #-}

------------------------------------------------------------------------------
-- Prism Combinators
------------------------------------------------------------------------------

-- | Build a 'Control.Lens.Prism.Prism'.
--
-- @'Either' t a@ is used instead of @'Maybe' a@ to permit the types of @s@ and @t@ to differ.
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = lmap (first pure . seta) . prismatic . rmap (fmap bt)
{-# INLINE prism #-}

-- | Build a 'Prism''.
prism' :: (a -> s) -> (s -> Maybe a) -> Prism' s a
prism' as sma = prism as (\s -> maybe (Left s) Right (sma s))
{-# INLINE prism' #-}

-- | Use a 'Prism' as a kind of first-class pattern.
--
-- @'outside' :: 'Prism' s t a b -> 'Lens' (t -> r) (s -> r) (b -> r) (a -> r)@
outside :: APrism s t a b -> Lens (t -> r) (s -> r) (b -> r) (a -> r)
outside k = case runPrism k of
  (bt, seta) -> \f tr -> f (tr.bt) <&> \ar -> either tr ar . seta
{-# INLINE outside #-}

-- | Use a 'Prism' to work over part of a structure.
aside :: APrism s t a b -> Prism (e, s) (e, t) (e, a) (e, b)
aside k = case runPrism k of
  (bt, seta) -> prism (fmap bt) $ \(e,s) -> case seta s of
    Left t -> Left (e,t)
    Right a -> Right (e,a)
{-# INLINE aside #-}

-- | Given a pair of prisms, project sums.
--
-- Viewing a 'Prism' as a co-lens, this combinator can be seen to be dual to 'alongside'.
without :: APrism s t a b
        -> APrism u v c d
        -> Prism (Either s u) (Either t v) (Either a c) (Either b d)
without k = case runPrism k of
  (bt, seta) -> \ k' -> case runPrism k' of
    (dv, uevc) -> prism (bimap bt dv) $ \su -> case su of
      Left s  -> bimap Left Left (seta s)
      Right u -> bimap Right Right (uevc u)
{-# INLINE without #-}

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
remit :: Reviewing s t a b -> Getter b t
remit p = to (runIdentity . reviewed . p . Review . Identity)
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
review :: MonadReader b m => Reviewing s t a b -> m t
review p = asks (runIdentity . reviewed . p . Review . Identity)
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
reviews :: MonadReader b m => Reviewing s t a b -> (t -> r) -> m r
reviews p tr = asks (tr . runIdentity . reviewed . p . Review . Identity)
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
reuse :: MonadState b m => Reviewing s t a b -> m t
reuse p = gets (runIdentity . reviewed . p . Review . Identity)
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
reuses :: MonadState b m => Reviewing s t a b -> (t -> r) -> m r
reuses p tr = gets (tr . runIdentity . reviewed . p . Review . Identity)
{-# INLINE reuses #-}

------------------------------------------------------------------------------
-- Common Prisms
------------------------------------------------------------------------------

-- | This prism provides a traversal for tweaking the left-hand value of an 'Either':
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
-- It also can be turned around to obtain the embedding into the 'Left' half of an 'Either':
--
-- >>> 5^.remit _left
-- Left 5
_left :: Prism (Either a c) (Either b c) a b
_left = prism Left $ either Right (Left . Right)
{-# INLINE _left #-}

-- | This prism provides a traversal for tweaking the right-hand value of an 'Either':
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
-- It also can be turned around to obtain the embedding into the 'Right' half of an 'Either':
--
-- >>> 5^.remit _right
-- Right 5
_right :: Prism (Either c a) (Either c b) a b
_right = prism Right $ either (Left . Left) Right
{-# INLINE _right #-}

-- | This prism provides a traversal for tweaking the target of the value of 'Just' in a 'Maybe'.
--
-- >>> over _just (+1) (Just 2)
-- Just 3
--
-- Unlike 'traverse' this is a 'Prism', and so you can use it to inject as well:
--
-- >>> 5^.remit _just
-- Just 5
_just :: Prism (Maybe a) (Maybe b) a b
_just = prism Just $ maybe (Left Nothing) Right
{-# INLINE _just #-}

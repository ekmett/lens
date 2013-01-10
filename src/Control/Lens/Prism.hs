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
  -- * Constructing Prisms
  , prism
  , prism'
  -- * Consuming Prisms
  , clonePrism
  , runPrism
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
import Control.Lens.Combinators
import Control.Lens.Internal
import Control.Lens.Type
import Data.Bifunctor
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

-- | If you see this in a signature for a function, the function is expecting a 'Prism'.
type APrism s t a b = Market a b a (Mutator b) -> Market a b s (Mutator t)

type APrism' s a = APrism s s a a

-- | Safely decompose 'APrism'.
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
-- See 'Control.Lens.Lens.cloneLens' and 'Control.Lens.Traversal.cloneTraversal' for examples of why you might want to do this.
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

-- | Given a pair of 'Prism's, project sums.
--
-- Viewing a 'Prism' as a co-'Lens', this combinator can be seen to be dual to 'Control.Lens.Lens.alongside'.
without :: APrism s t a b
        -> APrism u v c d
        -> Prism (Either s u) (Either t v) (Either a c) (Either b d)
without k = case runPrism k of
  (bt, seta) -> \ k' -> case runPrism k' of
    (dv, uevc) -> prism (bimap bt dv) $ \su -> case su of
      Left s  -> bimap Left Left (seta s)
      Right u -> bimap Right Right (uevc u)
{-# INLINE without #-}

------------------------------------------------------------------------------
-- Common Prisms
------------------------------------------------------------------------------

-- | This 'Prism' provides a traversal for tweaking the left-hand value of an 'Either':
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

-- | This 'Prism' provides a traversal for tweaking the right-hand value of an 'Either':
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

-- | This 'Prism' provides a traversal for tweaking the target of the value of 'Just' in a 'Maybe'.
--
-- >>> over _just (+1) (Just 2)
-- Just 3
--
-- Unlike 'Data.Traversable.traverse' this is a 'Prism', and so you can use it to inject as well:
--
-- >>> 5^.remit _just
-- Just 5
_just :: Prism (Maybe a) (Maybe b) a b
_just = prism Just $ maybe (Left Nothing) Right
{-# INLINE _just #-}

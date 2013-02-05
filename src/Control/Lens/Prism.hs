{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Prism
-- Copyright   :  (C) 2012-13 Edward Kmett
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
  , outside
  , aside
  , without
  , isn't
  -- * Common Prisms
  , _Left
  , _Right
  , _Just
  , _Nothing
  , _Void
  , only
  -- * Prismatic profunctors
  , Choice(..)
  ) where

import Control.Applicative
import Control.Lens.Combinators
import Control.Lens.Internal.Prism
import Control.Lens.Internal.Setter
import Control.Lens.Type
import Control.Monad
import Data.Bifunctor
import Data.Profunctor
import Data.Void
#ifndef SAFE
import Unsafe.Coerce
#endif

{-# ANN module "HLint: ignore Use camelCase" #-}

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import Numeric.Natural
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> let isLeft  (Left  _) = True; isLeft  _ = False
-- >>> let isRight (Right _) = True; isRight _ = False
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g

------------------------------------------------------------------------------
-- Prism Internals
------------------------------------------------------------------------------

-- | If you see this in a signature for a function, the function is expecting a 'Prism'.
type APrism s t a b = Market a b a (Mutator b) -> Market a b s (Mutator t)

-- | @
-- type APrism' = 'Simple' 'APrism'
-- @
type APrism' s a = APrism s s a a

-- | Convert 'APrism' to the pair of functions that characterize it.
runPrism :: APrism s t a b -> Market a b s t
#ifdef SAFE
runPrism k = case k (Market Mutator Right) of
  Market bt seta -> Market (runMutator #. bt) (either (Left . runMutator) Right . seta)
#else
runPrism k = unsafeCoerce (k (Market Mutator Right))
#endif
{-# INLINE runPrism #-}

-- | Clone a 'Prism' so that you can reuse the same monomorphically typed 'Prism' for different purposes.
--
-- See 'Control.Lens.Lens.cloneLens' and 'Control.Lens.Traversal.cloneTraversal' for examples of why you might want to do this.
clonePrism :: APrism s t a b -> Prism s t a b
clonePrism k = case runPrism k of
  Market bt seta -> prism bt seta
{-# INLINE clonePrism #-}

------------------------------------------------------------------------------
-- Prism Combinators
------------------------------------------------------------------------------

-- | Build a 'Control.Lens.Prism.Prism'.
--
-- @'Either' t a@ is used instead of @'Maybe' a@ to permit the types of @s@ and @t@ to differ.
--
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

-- | This is usually used to build a 'Prism'', when you have to use an operation like
-- 'Data.Typeable.cast' which already returns a 'Maybe'.
prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))
{-# INLINE prism' #-}

-- | Use a 'Prism' as a kind of first-class pattern.
--
-- @'outside' :: 'Prism' s t a b -> 'Lens' (t -> r) (s -> r) (b -> r) (a -> r)@
outside :: APrism s t a b -> Lens (t -> r) (s -> r) (b -> r) (a -> r)
outside k = case runPrism k of
  Market bt seta -> \f tr -> f (tr.bt) <&> \ar -> either tr ar . seta
{-# INLINE outside #-}

-- | Use a 'Prism' to work over part of a structure.
aside :: APrism s t a b -> Prism (e, s) (e, t) (e, a) (e, b)
aside k = case runPrism k of
  Market bt seta -> prism (fmap bt) $ \(e,s) -> case seta s of
    Left t -> Left (e,t)
    Right a -> Right (e,a)
{-# INLINE aside #-}

-- | Given a pair of prisms, project sums.
--
-- Viewing a 'Prism' as a co-'Lens', this combinator can be seen to be dual to 'Control.Lens.Lens.alongside'.
without :: APrism s t a b
        -> APrism u v c d
        -> Prism (Either s u) (Either t v) (Either a c) (Either b d)
without k = case runPrism k of
  Market bt seta -> \ k' -> case runPrism k' of
    Market dv uevc -> prism (bimap bt dv) $ \su -> case su of
      Left s  -> bimap Left Left (seta s)
      Right u -> bimap Right Right (uevc u)
{-# INLINE without #-}

-- | Check to see if this 'Prism' doesn't match.
--
-- >>> isn't _Left (Right 12)
-- True
--
-- >>> isn't _Left (Left 12)
-- False
isn't :: APrism s t a b -> s -> Bool
isn't k s = case runPrism k of
  Market _ seta -> case seta s of
    Left _ -> True
    Right _ -> False
{-# INLINE isn't #-}

------------------------------------------------------------------------------
-- Common Prisms
------------------------------------------------------------------------------

-- | This 'Prism' provides a 'Traversal' for tweaking the 'Left' half of an 'Either':
--
-- >>> over _Left (+1) (Left 2)
-- Left 3
--
-- >>> over _Left (+1) (Right 2)
-- Right 2
--
-- >>> Right 42 ^._Left :: String
-- ""
--
-- >>> Left "hello" ^._Left
-- "hello"
--
-- It also can be turned around to obtain the embedding into the 'Left' half of an 'Either':
--
-- >>> _Left # 5
-- Left 5
--
-- >>> 5^.re _Left
-- Left 5
_Left :: Prism (Either a c) (Either b c) a b
_Left = prism Left $ either Right (Left . Right)
{-# INLINE _Left #-}

-- | This 'Prism' provides a 'Traversal' for tweaking the 'Right' half of an 'Either':
--
-- >>> over _Right (+1) (Left 2)
-- Left 2
--
-- >>> over _Right (+1) (Right 2)
-- Right 3
--
-- >>> Right "hello" ^._Right
-- "hello"
--
-- >>> Left "hello" ^._Right :: [Double]
-- []
--
-- It also can be turned around to obtain the embedding into the 'Right' half of an 'Either':
--
-- >>> _Right # 5
-- Right 5
--
-- >>> 5^.re _Right
-- Right 5
_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right $ either (Left . Left) Right
{-# INLINE _Right #-}

-- | This 'Prism' provides a 'Traversal' for tweaking the target of the value of 'Just' in a 'Maybe'.
--
-- >>> over _Just (+1) (Just 2)
-- Just 3
--
-- Unlike 'Data.Traversable.traverse' this is a 'Prism', and so you can use it to inject as well:
--
-- >>> _Just # 5
-- Just 5
--
-- >>> 5^.re _Just
-- Just 5
--
-- Interestingly,
--
-- @
-- m '^?' '_Just' ≡ m
-- @
--
-- >>> Just x ^? _Just
-- Just x
--
-- >>> Nothing ^? _Just
-- Nothing
_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (Left Nothing) Right
{-# INLINE _Just #-}

-- | This 'Prism' provides the 'Traversal' of a 'Nothing' in a 'Maybe'.
--
-- >>> Nothing ^? _Nothing
-- Just ()
--
-- >>> Just () ^? _Nothing
-- Nothing
--
-- But you can turn it around and use it to construct 'Nothing' as well:
--
-- >>> _Nothing # ()
-- Nothing
_Nothing :: Prism' (Maybe a) ()
_Nothing = prism' (const Nothing) $ maybe (Just ()) (const Nothing)
{-# INLINE _Nothing #-}

-- | 'Void' is a logically uninhabited data type.
--
-- This is a 'Prism' that will always fail to match.
_Void :: Prism s s a Void
_Void = prism absurd Left
{-# INLINE _Void #-}

-- | This 'Prism' compares for exact equality with a given value.
--
-- >>> only 4 # ()
-- 4
--
-- >>> 5 ^? only 4
-- Nothing
only :: Eq a => a -> Prism' a ()
only a = prism' (\() -> a) $ guard . (a ==)
{-# INLINE only #-}

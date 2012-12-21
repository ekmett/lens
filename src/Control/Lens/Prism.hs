{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    Prism
  , Prism'
  , APrism
  -- * Constructing Prisms
  , prism
  , prism'
  -- * Consuming Prisms
  , Reviewing
  , clonePrism
  , withPrism
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
  -- * Implementation details
  , Prismatic(..)
  , Review(..)
  -- * Simple
  , SimplePrism
  ) where

import Control.Applicative
import Control.Arrow ((+++))
import Control.Monad.Reader as Reader
import Control.Monad.State as State 
import Control.Lens.Classes
import Control.Lens.Combinators
import Control.Lens.Getter
import Control.Lens.Internal
import Control.Lens.Type
import Data.Functor.Identity
import Data.Profunctor

{-# ANN module "HLint: ignore Collapse lambdas" #-}

-- $setup
-- >>> import Control.Lens
-- >>> import Numeric.Natural
-- >>> :set -XFlexibleContexts
-- >>> let nat :: Simple Prism Integer Natural; nat = prism toInteger $ \i -> if i <= 0 then Left i else Right (fromInteger i)
-- >>> let isLeft  (Left  _) = True; isLeft  _ = False
-- >>> let isRight (Right _) = True; isRight _ = False

------------------------------------------------------------------------------
-- Prism Internals
------------------------------------------------------------------------------

-- | A 'Prism' @l@ is a 0-or-1 target 'Traversal' that can also be turned around with 'remit' to
-- obtain a 'Getter' in the opposite direction.
--
-- There are two laws that a 'Prism' should satisfy:
--
-- First, if I 'remit' or 'review' a value with a 'Prism' and then 'preview' or use ('^?'), I will get it back:
--
-- * @'preview' l ('review' l b) ≡ 'Just' b@
--
-- Second, if you can extract a value @a@ using a Prism @l@ from a value @s@, then the value @s@ is completely described my @l@ and @a@:
--
-- * If @'preview' l s ≡ 'Just' a@ then @'review' l a ≡ s@
--
-- These two laws imply that the 'Traversal' laws hold for every 'Prism' and that we 'traverse' at most 1 element:
--
-- @'Control.Lens.Fold.lengthOf' l x '<=' 1@
--
-- It may help to think of this as a 'Control.Lens.Iso.Iso' that can be partial in one direction.
--
-- Every 'Prism' is a valid 'Traversal'.
--
-- Every 'Control.Lens.Iso.Iso' is a valid 'Prism'.
--
-- For example, you might have a @'Prism'' 'Integer' Natural@ allows you to always
-- go from a 'Natural' to an 'Integer', and provide you with tools to check if an 'Integer' is
-- a 'Natural' and/or to edit one if it is.
--
--
-- @
-- 'nat' :: 'Prism'' 'Integer' 'Numeric.Natural.Natural'
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
-- -- a /co/-'Lens', so to speak. This is what permits the construction of 'outside'.
type Prism s t a b = forall k f. (Prismatic k, Applicative f) => k a (f b) -> k s (f t)

-- | A 'Simple' 'Prism'
type Prism' s a = Prism s s a a

-- FIXME: Should we use another name for Identity?
type Reviewing s t a b = Overloading Review Review Identity s t a b

-- | If you see this in a signature for a function, the function is expecting a 'Prism'.
type APrism s t a b = Market a a (Identity b) -> Market a s (Identity t)

-- | Safely decompose 'APrism'
withPrism :: ((b -> t) -> (s -> Either t a) -> r) -> APrism s t a b -> r
withPrism k p = k bt seta where
  go = p . Market Right . Identity
  {-# INLINE go #-}
  bt = runIdentity . extort . go
  seta = either (Left . runIdentity) Right . market (go (error "withPrism: invalid Prism passed as APrism"))
{-# INLINE withPrism #-}

-- | Clone a 'Prism' so that you can reuse the same monomorphically typed 'Prism' for different purposes.
--
-- See 'cloneLens' and 'cloneTraversal' for examples of why you might want to do this.
clonePrism :: APrism s t a b -> Prism s t a b
clonePrism = withPrism prism
{-# INLINE clonePrism #-}

------------------------------------------------------------------------------
-- Prism Combinators
------------------------------------------------------------------------------

-- | Build a 'Control.Lens.Prism.Prism'.
--
-- @'Either' t a@ is used instead of @'Maybe' a@ to permit the types of @s@ and @t@ to differ.
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = prismatic seta . rmap (fmap bt)
{-# INLINE prism #-}

-- | Build a 'Prism''.
prism' :: (a -> s) -> (s -> Maybe a) -> Prism' s a
prism' as sma = prism as (\s -> maybe (Left s) Right (sma s))
{-# INLINE prism' #-}

-- | Use a 'Prism' as a kind of first-class pattern.
--
-- @'outside' :: 'Prism' s t a b -> 'Lens' (t -> r) (s -> r) (b -> r) (a -> r)@
outside :: APrism s t a b -> Lens (t -> r) (s -> r) (b -> r) (a -> r)
outside = withPrism $ \bt seta -> \f tr -> f (tr.bt) <&> \ar -> either tr ar . seta
{-# INLINE outside #-}

-- | Use a 'Prism' to work over part of a structure.
aside :: APrism s t a b -> Prism (e, s) (e, t) (e, a) (e, b)
aside = withPrism $ \bt seta -> prism (fmap bt) $ \(e,s) -> case seta s of
  Left t -> Left (e,t)
  Right a -> Right (e,a)
{-# INLINE aside #-}

-- | Given a pair of prisms, project sums.
--
-- Viewing a 'Prism' as a co-lens, this combinator can be seen to be dual to 'alongside'.
without :: APrism s t a b
        -> APrism u v c d
        -> Prism (Either s u) (Either t v) (Either a c) (Either b d)
without = withPrism $ \bt seta -> withPrism $ \dv uevc ->
  let go (Left s) = either (Left . Left) (Right . Left) (seta s)
      go (Right u) = either (Left . Right) (Right . Right) (uevc u)
  in prism (bt +++ dv) go
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
--
-- (Unfortunately the instance for
-- @'Data.Traversable.Traversable' ('Either' c)@ is still missing from base,
-- so this can't just be 'Data.Traversable.traverse'.)
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

-- | A deprecated alias for @'Prism''@.
type SimplePrism s a = Prism s s a a
{-# DEPRECATED SimplePrism "use Prism'" #-}

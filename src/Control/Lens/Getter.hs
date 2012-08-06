{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Getter
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
--
-- A @'Getter' a c@ is just any function @(a -> c)@, which we've flipped into continuation
-- passing style, @(c -> r) -> a -> r@ and decorated with 'Accessor' to obtain
--
-- > type Getting r a b c d = (c -> Accessor r d) -> a -> Accessor r b
--
-- If we restrict access to knowledge about the type 'r' and can work for any d and b, we could get:
--
-- > type Getter a c = forall r b d. Getting r a b c d
--
-- But we actually hide the use of 'Accessor' behind a class 'Gettable' to error messages from
-- type class resolution rather than at unification time, where they are much uglier.
-- type Getter a c = forall f b d. Gettable f => (c -> f d) -> a -> f b
--
-- Everything you can do with a function, you can do with a 'Getter', but note that because of the
-- continuation passing style @(.)@ composes them in the opposite order.
--
-- Since it is only a function, every 'Getter' obviously only retrieves a single value for a given
-- input.
--
----------------------------------------------------------------------------
module Control.Lens.Getter
  (
  -- * Getters
    Getter
  , Getting
  , Gettable(..)
  , Accessor(..)
  -- * Building Getters
  , to
  -- * Combinators for Getters and Folds
  , (^.), (^$)
  , view
  , views
  , use
  , uses
  , query
  , queries
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Lens.Internal
import Control.Monad.Reader.Class       as Reader
import Control.Monad.State.Class        as State
import Data.Complex -- for tests
import Data.Functor.Compose
import Data.Monoid
import Control.Lens.Type -- for tests

infixl 8 ^.
infixr 0 ^$

---------------
-- Getters
---------------

-- | A 'Getter' describes how to retrieve a single value in a way that can be composed with
-- other lens-like constructions.
--
-- Unlike a 'Lens' a 'Getter' is read-only. Since a 'Getter' cannot be used to write back
-- there are no lens laws that can be applied to it.
--
-- Moreover, a 'Getter' can be used directly as a 'Fold', since it just ignores the 'Monoid'.
type Getter a c = forall f b d. Gettable f => (c -> f d) -> a -> f b

-- | Build a 'Getter' from an arbitrary Haskell function.
--
-- > to f . to g = to (g . f)
-- > a^.to f = f a
--
-- >>> (0, -5)^._2.to abs
-- 5
to :: (a -> c) -> Getter a c
to f g = coerce . g . f
{-# INLINE to #-}

-- |
-- Most 'Getter' combinators are able to be used with both a 'Getter' or a 'Fold' in
-- limited situations, to do so, they need to be monomorphic in what we are going to
-- extract with 'Const'. To be compatible with 'Lens', 'Traversal' and 'Iso' we also
-- restricted choices of the irrelevant b and d parameters.
--
-- If a function accepts a @Getting m r a b c d@, then when @r@ is a Monoid, and @m@ is a
-- 'Monad' you can pass a 'Fold' (or 'Traversal'), otherwise you can only pass this a
-- 'Getter' or 'Lens'.
type Getting r a b c d = (c -> Accessor r d) -> a -> Accessor r b

-----------------------------------------------------------------------------
-- Gettables & Accessors
-----------------------------------------------------------------------------

-- | Generalizing Const so we can apply simple Applicative transformations to it
-- and so we can get nicer error messages
class Functor f => Gettable f where
  coerce :: f a -> f b

instance Gettable (Const r) where
  coerce (Const m) = Const m

instance Gettable f => Gettable (Backwards f) where
  coerce = Backwards . coerce . forwards

instance (Functor f, Gettable g) => Gettable (Compose f g) where
  coerce = Compose . fmap coerce . getCompose

instance Gettable f => Gettable (ElementOf f) where
  coerce (ElementOf m) = ElementOf $ \i -> case m i of
    Searching _ _ -> NotFound "coerced while searching"
    Found j as    -> Found j (coerce as)
    NotFound s    -> NotFound s

-- | Used instead of Const to report 'no instance of (Settable Accessor)' when
-- attempting to misuse a 'Setter' as a 'Getter'.
newtype Accessor r a = Accessor { runAccessor :: r }

instance Functor (Accessor r) where
  fmap _ (Accessor m) = Accessor m

instance Gettable (Accessor r) where
  coerce (Accessor m) = Accessor m

instance Monoid r => Applicative (Accessor r) where
  pure _ = Accessor mempty
  Accessor a <*> Accessor b = Accessor (mappend a b)

-------------------------------
-- Getting Values
-------------------------------

-- | View the value pointed to by a 'Getter', 'Iso' or 'Lens' or the result of folding over
-- all the results of a 'Fold' or 'Traversal' that points at a monoidal values.
--
-- It may be useful to think of 'view' as having these more restrictive signatures:
--
-- > view . to = id
--
-- >>> view _2 (1,"hello")
-- "hello"
--
-- > view ::             Getter a c          -> a -> c
-- > view :: Monoid m => Fold a m            -> a -> m
-- > view ::             Iso a b c d         -> a -> c
-- > view ::             Lens a b c d        -> a -> c
-- > view :: Monoid m => Traversal a b m d   -> a -> m
view :: Getting c a b c d -> a -> c
view l = runAccessor . l Accessor
{-# INLINE view #-}

-- | View the value of a 'Getter', 'Iso', 'Lens' or the result of folding over the
-- result of mapping the targets of a 'Fold' or 'Traversal'.
--
-- It may be useful to think of 'views' as having these more restrictive signatures:
--
-- >>> views _2 length (1,"hello")
-- 5
--
-- > views ::             Getter a c          -> (c -> d) -> a -> d
-- > views :: Monoid m => Fold a c            -> (c -> m) -> a -> m
-- > views ::             Iso a b c d         -> (c -> d) -> a -> d
-- > views ::             Lens a b c d        -> (c -> d) -> a -> d
-- > views :: Monoid m => Traversal a b c d   -> (c -> m) -> a -> m
views :: Getting m a b c d -> (c -> m) -> a -> m
views l f = runAccessor . l (Accessor . f)
{-# INLINE views #-}

-- | View the value pointed to by a 'Getter', 'Iso' or 'Lens' or the result of folding over
-- all the results of a 'Fold' or 'Traversal' that points at a monoidal values.
--
-- This is the same operation as 'view', only infix.
--
-- >>> _2 ^$ (1, "hello")
-- "hello"
--
-- > (^$) ::             Getter a c          -> a -> c
-- > (^$) :: Monoid m => Fold a m            -> a -> m
-- > (^$) ::             Iso a b c d         -> a -> c
-- > (^$) ::             Lens a b c d        -> a -> c
-- > (^$) :: Monoid m => Traversal a b m d   -> a -> m
(^$) :: Getting c a b c d -> a -> c
l ^$ a = runAccessor (l Accessor a)
{-# INLINE (^$) #-}

-- | View the value pointed to by a 'Getter' or 'Lens' or the result of folding over
-- all the results of a 'Fold' or 'Traversal' that points at a monoidal values.
--
-- This is the same operation as 'view' with the arguments flipped.
--
-- The fixity and semantics are such that subsequent field accesses can be
-- performed with (Prelude..)
--
-- >>> ((0, 1 :+ 2), 3)^._1._2.to magnitude
-- 2.23606797749979
--
-- > (^.) ::             a -> Getter a c          -> c
-- > (^.) :: Monoid m => a -> Fold a m            -> m
-- > (^.) ::             a -> Iso a b c d         -> c
-- > (^.) ::             a -> Lens a b c d        -> c
-- > (^.) :: Monoid m => a -> Traversal a b m d   -> m
(^.) :: a -> Getting c a b c d -> c
a ^. l = runAccessor (l Accessor a)
{-# INLINE (^.) #-}

------------------------------------------------------------------------------
-- MonadReader
------------------------------------------------------------------------------

-- |
-- Query the target of a 'Lens', 'Iso' or 'Getter' in the current state, or use a
-- summary of a 'Fold' or 'Traversal' that points to a monoidal value.
--
-- > query :: MonadReader a m             => Getter a c        -> m c
-- > query :: (MonadReader a m, Monoid c) => Fold a c          -> m c
-- > query :: MonadReader a m             => Iso a b c d       -> m c
-- > query :: MonadReader a m             => Lens a b c d      -> m c
-- > query :: (MonadReader a m, Monoid c) => Traversal a b c d -> m c
--
-- > query :: MonadReader a m => ((c -> Const c d) -> a -> Const c b) -> m c
query :: MonadReader a m => Getting c a b c d -> m c
query l = Reader.asks (^.l)
{-# INLINE query #-}

-- |
-- Use the target of a 'Lens', 'Iso' or 'Getter' in the current state, or use a
-- summary of a 'Fold' or 'Traversal' that points to a monoidal value.
--
-- > queries :: MonadReader a m             => Getter a c        -> (c -> e) -> m e
-- > queries :: (MonadReader a m, Monoid c) => Fold a c          -> (c -> e) -> m e
-- > queries :: MonadReader a m             => Iso a b c d       -> (c -> e) -> m e
-- > queries :: MonadReader a m             => Lens a b c d      -> (c -> e) -> m e
-- > queries :: (MonadReader a m, Monoid c) => Traversal a b c d -> (c -> e) -> m e
--
-- > queries :: MonadReader a m => ((c -> Const e d) -> a -> Const e b) -> (c -> e) -> m e
queries :: MonadReader a m => Getting e a b c d -> (c -> e) -> m e
queries l f = Reader.asks (views l f)
{-# INLINE queries #-}

------------------------------------------------------------------------------
-- MonadState
------------------------------------------------------------------------------

-- |
-- Use the target of a 'Lens', 'Iso', or 'Getter' in the current state, or use a
-- summary of a 'Fold' or 'Traversal' that points to a monoidal value.
--
-- > use :: MonadState a m             => Action m a b      -> m b
-- > use :: MonadState a m             => Getter a c        -> m c
-- > use :: (MonadState a m, Monoid r) => Fold a r          -> m r
-- > use :: MonadState a m             => Iso a b c d       -> m c
-- > use :: MonadState a m             => Lens a b c d      -> m c
-- > use :: (MonadState a m, Monoid r) => Traversal a b r d -> m r
use :: MonadState a m => Getting c a b c d -> m c
use l = State.gets (view l)
{-# INLINE use #-}

-- |
-- Use the target of a 'Lens', 'Iso' or 'Getter' in the current state, or use a
-- summary of a 'Fold' or 'Traversal' that points to a monoidal value.
--
-- > uses :: MonadState a m             => Action m a c      -> (c -> e) -> m e
-- > uses :: MonadState a m             => Getter a c        -> (c -> e) -> m e
-- > uses :: (MonadState a m, Monoid r) => Fold a c          -> (c -> r) -> m r
-- > uses :: MonadState a m             => Lens a b c d      -> (c -> e) -> m e
-- > uses :: MonadState a m             => Iso a b c d       -> (c -> e) -> m e
-- > uses :: (MonadState a m, Monoid r) => Traversal a b c d -> (c -> r) -> m r
uses :: MonadState a m => Getting e a b c d -> (c -> e) -> m e
uses l f = State.gets (views l f)
{-# INLINE uses #-}

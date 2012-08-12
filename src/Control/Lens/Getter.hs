{-# LANGUAGE Rank2Types #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Getter
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
--
-- A @'Getter' a c@ is just any function @(a -> c)@, which we've flipped
-- into continuation passing style, @(c -> r) -> a -> r@ and decorated
-- with 'Accessor' to obtain:
--
-- @type 'Getting' r a c = (c -> 'Accessor' r c) -> a -> 'Accessor' r a@
--
-- If we restrict access to knowledge about the type 'r' and can work for
-- any d and b, we could get:
--
-- @type 'Getter' a c = forall r. 'Getting' r a c@
--
-- But we actually hide the use of 'Accessor' behind a class 'Gettable'
-- to error messages from type class resolution rather than at unification
-- time, where they are much uglier.
--
-- @type 'Getter' a c = forall f. 'Gettable' f => (c -> f c) -> a -> f a@
--
-- Everything you can do with a function, you can do with a 'Getter', but
-- note that because of the continuation passing style ('.') composes them
-- in the opposite order.
--
-- Since it is only a function, every 'Getter' obviously only retrieves a
-- single value for a given input.
--
-------------------------------------------------------------------------------
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
import Data.Functor.Compose
import Data.Monoid

infixl 8 ^.
infixr 0 ^$

-------------------------------------------------------------------------------
-- Getters
-------------------------------------------------------------------------------

-- | A 'Getter' describes how to retrieve a single value in a way that can be
-- composed with other lens-like constructions.
--
-- Unlike a 'Control.Lens.Type.Lens' a 'Getter' is read-only. Since a 'Getter'
-- cannot be used to write back there are no lens laws that can be applied to
-- it. In fact, it is isomorphic to an arbitrary function from @(a -> c)@.
--
-- Moreover, a 'Getter' can be used directly as a 'Control.Lens.Fold.Fold',
-- since it just ignores the 'Applicative'.
type Getter a c = forall f. Gettable f => (c -> f c) -> a -> f a

-- | Build a 'Getter' from an arbitrary Haskell function.
--
-- @'to' f . 'to' g = 'to' (g . f)@
--
-- @a '^.' 'to' f = f a@
--
-- >>> import Control.Lens
-- >>> (0, -5)^._2.to abs
-- 5
to :: (a -> c) -> Getter a c
to f g = coerce . g . f
{-# INLINE to #-}

-- |
-- Most 'Getter' combinators are able to be used with both a 'Getter' or a
-- 'Control.Lens.Fold.Fold' in limited situations, to do so, they need to be
-- monomorphic in what we are going to extract with 'Const'. To be compatible
-- with 'Control.Lens.Type.Lens', 'Control.Lens.Traversal.Traversal' and
-- 'Control.Lens.Iso.Iso' we also restricted choices of the irrelevant @b@ and
-- @d@ parameters.
--
-- If a function accepts a @'Getting' r a c@, then when @r@ is a 'Monoid', then
-- you can pass a 'Control.Lens.Fold.Fold' (or
-- 'Control.Lens.Traversal.Traversal'), otherwise you can only pass this a
-- 'Getter' or 'Control.Lens.Type.Lens'.
type Getting r a c = (c -> Accessor r c) -> a -> Accessor r a

-------------------------------------------------------------------------------
-- Gettables & Accessors
-------------------------------------------------------------------------------

-- | Generalizing 'Const' so we can apply simple 'Applicative'
-- transformations to it and so we can get nicer error messages
--
-- A 'Gettable' 'Functor' ignores its argument, which it carries solely as a
-- phantom type parameter.
--
-- To ensure this, an instance of 'Gettable' is required to satisfy:
--
-- @'id' = 'fmap' f = 'coerce'@
class Functor f => Gettable f where
  -- | Replace the phantom type argument.
  coerce :: f a -> f b

instance Gettable (Const r) where
  coerce (Const m) = Const m

instance Gettable f => Gettable (Backwards f) where
  coerce = Backwards . coerce . forwards

instance (Functor f, Gettable g) => Gettable (Compose f g) where
  coerce = Compose . fmap coerce . getCompose

-- | This instance is a lie, but it is a useful lie.
instance Gettable f => Gettable (ElementOf f) where
  coerce (ElementOf m) = ElementOf $ \i -> case m i of
    Searching _ _ -> NotFound "coerced while searching" -- er...
    Found j as    -> Found j (coerce as)
    NotFound s    -> NotFound s

-- | Used instead of 'Const' to report
--
-- @No instance of ('Control.Lens.Setter.Settable' 'Accessor')@
--
-- when the user attempts to misuse a 'Control.Lens.Setter.Setter' as a
-- 'Getter', rather than a monolithic unification error.
newtype Accessor r a = Accessor { runAccessor :: r }

instance Functor (Accessor r) where
  fmap _ (Accessor m) = Accessor m

instance Gettable (Accessor r) where
  coerce (Accessor m) = Accessor m

instance Monoid r => Applicative (Accessor r) where
  pure _ = Accessor mempty
  Accessor a <*> Accessor b = Accessor (mappend a b)

-------------------------------------------------------------------------------
-- Getting Values
-------------------------------------------------------------------------------

-- | View the value pointed to by a 'Getter', 'Control.Lens.Iso.Iso' or
-- 'Control.Lens.Type.Lens' or the result of folding over all the results of a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Traversal.Traversal' that points
-- at a monoidal values.
--
-- @'view' . 'to' = 'id'@
--
-- >>> import Control.Lens
-- >>> view _2 (1,"hello")
-- "hello"
--
-- It may be useful to think of 'view' as having these more restrictive
-- signatures:
--
-- @
-- view ::             'Getter' a c             -> a -> c
-- view :: 'Monoid' m => 'Control.Lens.Fold.Fold' a m               -> a -> m
-- view ::             'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a c         -> a -> c
-- view ::             'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a c        -> a -> c
-- view :: 'Monoid' m => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a m   -> a -> m
-- @
view :: Getting c a c -> a -> c
view l = runAccessor . l Accessor
{-# INLINE view #-}

-- | View the value of a 'Getter', 'Control.Lens.Iso.Iso',
-- 'Control.Lens.Type.Lens' or the result of folding over the result of mapping
-- the targets of a 'Control.Lens.Fold.Fold' or
-- 'Control.Lens.Traversal.Traversal'.
--
-- It may be useful to think of 'views' as having these more restrictive
-- signatures:
--
-- >>> import Control.Lens
-- >>> views _2 length (1,"hello")
-- 5
--
-- @
-- views ::             'Getter' a c             -> (c -> d) -> a -> d
-- views :: 'Monoid' m => 'Control.Lens.Fold.Fold' a c               -> (c -> m) -> a -> m
-- views ::             'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a c         -> (c -> d) -> a -> d
-- views ::             'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a c        -> (c -> d) -> a -> d
-- views :: 'Monoid' m => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a c   -> (c -> m) -> a -> m
-- @
views :: Getting m a c -> (c -> m) -> a -> m
views l f = runAccessor . l (Accessor . f)
{-# INLINE views #-}

-- | View the value pointed to by a 'Getter', 'Control.Lens.Iso.Iso' or
-- 'Control.Lens.Type.Lens' or the result of folding over all the results of a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Traversal.Traversal' that points
-- at a monoidal values.
--
-- This is the same operation as 'view', only infix.
--
-- >>> import Control.Lens
-- >>> _2 ^$ (1, "hello")
-- "hello"
--
-- @
-- (^$) ::             'Getter' a c             -> a -> c
-- (^$) :: 'Monoid' m => 'Control.Lens.Fold.Fold' a m               -> a -> m
-- (^$) ::             'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a c         -> a -> c
-- (^$) ::             'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a c        -> a -> c
-- (^$) :: 'Monoid' m => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a m   -> a -> m
-- @
(^$) :: Getting c a c -> a -> c
l ^$ a = runAccessor (l Accessor a)
{-# INLINE (^$) #-}

-- | View the value pointed to by a 'Getter' or 'Control.Lens.Type.Lens' or the
-- result of folding over all the results of a 'Control.Lens.Fold.Fold' or
-- 'Control.Lens.Traversal.Traversal' that points at a monoidal values.
--
-- This is the same operation as 'view' with the arguments flipped.
--
-- The fixity and semantics are such that subsequent field accesses can be
-- performed with ('Prelude..')
--
-- >>> :m + Data.Complex Control.Lens
-- >>> ((0, 1 :+ 2), 3)^._1._2.to magnitude
-- 2.23606797749979
--
-- @
-- (^.) ::             a -> 'Getter' a c             -> c
-- (^.) :: 'Monoid' m => a -> 'Control.Lens.Fold.Fold' a m               -> m
-- (^.) ::             a -> 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a c         -> c
-- (^.) ::             a -> 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a c        -> c
-- (^.) :: 'Monoid' m => a -> 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a m   -> m
-- @
(^.) :: a -> Getting c a c -> c
a ^. l = runAccessor (l Accessor a)
{-# INLINE (^.) #-}

-------------------------------------------------------------------------------
-- MonadReader
-------------------------------------------------------------------------------

-- |
-- Query the target of a 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso' or
-- 'Getter' in the current state, or use a summary of a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Traversal.Traversal' that points
-- to a monoidal value.
--
-- @
-- query :: 'MonadReader' a m             => 'Getter' a c           -> m c
-- query :: ('MonadReader' a m, 'Monoid' c) => 'Control.Lens.Fold.Fold' a c             -> m c
-- query :: 'MonadReader' a m             => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a c       -> m c
-- query :: 'MonadReader' a m             => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a c      -> m c
-- query :: ('MonadReader' a m, 'Monoid' c) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a c -> m c
-- @
query :: MonadReader a m => Getting c a c -> m c
query l = Reader.asks (^.l)
{-# INLINE query #-}

-- |
-- Use the target of a 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso' or
-- 'Getter' in the current state, or use a summary of a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Traversal.Traversal' that points
-- to a monoidal value.
--
-- @
-- queries :: 'MonadReader' a m             => 'Getter' a c           -> (c -> e) -> m e
-- queries :: ('MonadReader' a m, 'Monoid' c) => 'Control.Lens.Fold.Fold' a c             -> (c -> e) -> m e
-- queries :: 'MonadReader' a m             => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a c       -> (c -> e) -> m e
-- queries :: 'MonadReader' a m             => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a c      -> (c -> e) -> m e
-- queries :: ('MonadReader' a m, 'Monoid' c) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a c -> (c -> e) -> m e
-- @
queries :: MonadReader a m => Getting e a c -> (c -> e) -> m e
queries l f = Reader.asks (views l f)
{-# INLINE queries #-}

-------------------------------------------------------------------------------
-- MonadState
-------------------------------------------------------------------------------

-- |
-- Use the target of a 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', or
-- 'Getter' in the current state, or use a summary of a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Traversal.Traversal' that points
-- to a monoidal value.
--
-- @
-- use :: 'MonadState' a m             => 'Getter' a c             -> m c
-- use :: ('MonadState' a m, 'Monoid' r) => 'Control.Lens.Fold.Fold' a r               -> m r
-- use :: 'MonadState' a m             => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a c         -> m c
-- use :: 'MonadState' a m             => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a c        -> m c
-- use :: ('MonadState' a m, 'Monoid' r) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a r   -> m r
-- @
use :: MonadState a m => Getting c a c -> m c
use l = State.gets (view l)
{-# INLINE use #-}

-- |
-- Use the target of a 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso' or
-- 'Getter' in the current state, or use a summary of a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Traversal.Traversal' that
-- points to a monoidal value.
--
-- @
-- uses :: 'MonadState' a m             => 'Getter' a c           -> (c -> e) -> m e
-- uses :: ('MonadState' a m, 'Monoid' r) => 'Control.Lens.Fold.Fold' a c             -> (c -> r) -> m r
-- uses :: 'MonadState' a m             => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a c      -> (c -> e) -> m e
-- uses :: 'MonadState' a m             => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a c       -> (c -> e) -> m e
-- uses :: ('MonadState' a m, 'Monoid' r) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a c -> (c -> r) -> m r
-- @
uses :: MonadState a m => Getting e a c -> (c -> e) -> m e
uses l f = State.gets (views l f)
{-# INLINE uses #-}

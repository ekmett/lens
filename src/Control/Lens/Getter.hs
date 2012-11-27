{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
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
-- A @'Getter' s a@ is just any function @(s -> a)@, which we've flipped
-- into continuation passing style, @(a -> r) -> s -> r@ and decorated
-- with 'Accessor' to obtain:
--
-- @type 'Getting' r s t a b = (a -> 'Accessor' r b) -> s -> 'Accessor' r t@
--
-- If we restrict access to knowledge about the type 'r' and can work for
-- any b and t, we could get:
--
-- @type 'Getter' s a = forall r. 'Getting' r s s a a@
--
-- But we actually hide the use of 'Accessor' behind a class 'Gettable'
-- to error messages from type class resolution rather than at unification
-- time, where they are much uglier.
--
-- @type 'Getter' s a = forall f. 'Gettable' f => (a -> f a) -> s -> f s@
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
  -- * Building Getters
  , to
  -- * Combinators for Getters and Folds
  , (^.), (^$)
  , (&), (^&)
  , view
  , views
  , use
  , uses

  -- * Storing Getters
  , ReifiedGetter(..)
  , Gettable
  , Accessor

  ) where

import Control.Lens.Classes
import Control.Lens.Internal
import Control.Lens.Internal.Combinators
import Control.Monad.Reader.Class as Reader
import Control.Monad.State        as State

-- $setup
-- >>> import Control.Lens
-- >>> import Data.List.Lens

infixl 8 ^., ^&
infixl 1 &
infixr 0 ^$

-------------------------------------------------------------------------------
-- Pipelining
-------------------------------------------------------------------------------

-- | Passes the result of the left side to the function on the right side (forward pipe operator).
--
-- This is the flipped version of ('$'), which is more common in languages like F# as (@|>@) where it is needed
-- for inference. Here it is supplied for notational convenience and given a precedence that allows it
-- to be nested inside uses of ('$').
--
-- >>> "hello" & length & succ
-- 6
--
-- This combinator is commonly used when applying multiple lens operations in sequence.
--
-- >>> ("hello","world") & _1.element 0 .~ 'j' & _1.element 4 .~ 'y'
-- ("jelly","world")
--
-- This reads somewhat similar to:
--
-- >>> flip execState ("hello","world") $ do _1.element 0 .= 'j'; _1.element 4 .= 'y'
-- ("jelly","world")

(&) :: a -> (a -> b) -> b
a & f = f a
{-# INLINE (&) #-}

-- | A version of ('&') with much tighter precedence that can be interleaved with ('^.')
--
-- >>> "hello" ^& length
-- 5
--
-- >>> ("hello","world")^._1^&reverse^?!_head
-- 'o'
(^&) :: a -> (a -> b) -> b
a ^& f = f a

-------------------------------------------------------------------------------
-- Getters
-------------------------------------------------------------------------------

-- | A 'Getter' describes how to retrieve a single value in a way that can be
-- composed with other lens-like constructions.
--
-- Unlike a 'Control.Lens.Type.Lens' a 'Getter' is read-only. Since a 'Getter'
-- cannot be used to write back there are no lens laws that can be applied to
-- it. In fact, it is isomorphic to an arbitrary function from @(a -> s)@.
--
-- Moreover, a 'Getter' can be used directly as a 'Control.Lens.Fold.Fold',
-- since it just ignores the 'Applicative'.
type Getter s a = forall f. Gettable f => (a -> f a) -> s -> f s

-- | Build a 'Getter' from an arbitrary Haskell function.
--
-- @'to' f . 'to' g ≡ 'to' (g . f)@
--
-- @a '^.' 'to' f ≡ f a@
--
--
-- >>> ("hello","world")^.to snd
-- "world"
--
-- >>> 5^.to succ
-- 6
--
-- >>> (0, -5)^._2.to abs
-- 5
to :: (s -> a) -> Getter s a
to f g = coerce . g . f
{-# INLINE to #-}


-- |
-- When you see this in a type signature it indicates that you can
-- pass the function a 'Control.Lens.Type.Lens', 'Getter',
-- 'Control.Lens.Traversal.Traversal', 'Control.Lens.Fold.Fold',
-- 'Control.Lens.Projection.Projection', 'Control.Lens.Iso.Iso',
-- or one of the indexed variants, and it will just \"do the right thing\".
--
-- Most 'Getter' combinators are able to be used with both a 'Getter' or a
-- 'Control.Lens.Fold.Fold' in limited situations, to do so, they need to be
-- monomorphic in what we are going to extract with 'Const'. To be compatible
-- with 'Control.Lens.Type.Lens', 'Control.Lens.Traversal.Traversal' and
-- 'Control.Lens.Iso.Iso' we also restricted choices of the irrelevant @t@ and
-- @b@ parameters.
--
-- If a function accepts a @'Getting' r s t a b@, then when @r@ is a 'Monoid', then
-- you can pass a 'Control.Lens.Fold.Fold' (or
-- 'Control.Lens.Traversal.Traversal'), otherwise you can only pass this a
-- 'Getter' or 'Control.Lens.Type.Lens'.
--
type Getting r s t a b = (a -> Accessor r b) -> s -> Accessor r t

-------------------------------------------------------------------------------
-- Getting Values
-------------------------------------------------------------------------------

-- | View the value pointed to by a 'Getter', 'Control.Lens.Iso.Iso' or
-- 'Control.Lens.Type.Lens' or the result of folding over all the results of a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Traversal.Traversal' that points
-- at a monoidal values.
--
-- @'view' . 'to' ≡ 'id'@
--
-- >>> view _2 (1,"hello")
-- "hello"
--
-- >>> view (to succ) 5
-- 6
--
-- >>> view (_2._1) ("hello",("world","!!!"))
-- "world"
--
--
-- As @views@ is commonly used to access the target of a 'Getter' or obtain a monoidal summary of the targets of a 'Fold',
-- It may be useful to think of it as having one of these more restrictive signatures:
--
-- @
-- 'view' ::             'Getter' s a             -> s -> a
-- 'view' :: 'Monoid' m => 'Control.Lens.Fold.Fold' s m               -> s -> m
-- 'view' ::             'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a         -> s -> a
-- 'view' ::             'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a        -> s -> a
-- 'view' :: 'Monoid' m => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s m   -> s -> m
-- @
--
-- In a more general setting, such as when working with a monad transformer stack you can use:
--
-- @
-- 'view' :: 'MonadReader' s m             => 'Getter' s a           -> m a
-- 'view' :: ('MonadReader' s m, 'Monoid' a) => 'Control.Lens.Fold.Fold' s a             -> m a
-- 'view' :: 'MonadReader' s m             => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a       -> m a
-- 'view' :: 'MonadReader' s m             => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a      -> m a
-- 'view' :: ('MonadReader' s m, 'Monoid' a) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> m a
-- @
view :: MonadReader s m => Getting a s t a b -> m a
view l = Reader.asks (runAccessor# (l Accessor))
{-# INLINE view #-}

-- | View the value of a 'Getter', 'Control.Lens.Iso.Iso',
-- 'Control.Lens.Type.Lens' or the result of folding over the result of mapping
-- the targets of a 'Control.Lens.Fold.Fold' or
-- 'Control.Lens.Traversal.Traversal'.
--
-- It may be useful to think of 'views' as having these more restrictive
-- signatures:
--
-- @'views' l f ≡ 'view' (l '.' 'to' f)@
--
-- >>> views _2 length (1,"hello")
-- 5
--
-- As @views@ is commonly used to access the target of a 'Getter' or obtain a monoidal summary of the targets of a 'Fold',
-- It may be useful to think of it as having one of these more restrictive signatures:
--
-- @
-- 'views' ::             'Getter' s a             -> (a -> r) -> s -> r
-- 'views' :: 'Monoid' m => 'Control.Lens.Fold.Fold' s a               -> (a -> m) -> s -> m
-- 'views' ::             'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a         -> (a -> r) -> s -> r
-- 'views' ::             'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a        -> (a -> r) -> s -> r
-- 'views' :: 'Monoid' m => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a   -> (a -> m) -> s -> m
-- @
--
-- In a more general setting, such as when working with a monad transformer stack you can use:
--
-- @
-- 'view' :: 'MonadReader' s m             => 'Getter' s a           -> m a
-- 'view' :: ('MonadReader' s m, 'Monoid' a) => 'Control.Lens.Fold.Fold' s a             -> m a
-- 'view' :: 'MonadReader' s m             => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a       -> m a
-- 'view' :: 'MonadReader' s m             => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a      -> m a
-- 'view' :: ('MonadReader' s m, 'Monoid' a) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> m a
-- @
views :: MonadReader s m => Getting r s t a b -> (a -> r) -> m r
views l f = Reader.asks (runAccessor# (l (accessor# f)))
{-# INLINE views #-}

-- | View the value pointed to by a 'Getter', 'Control.Lens.Iso.Iso' or
-- 'Control.Lens.Type.Lens' or the result of folding over all the results of a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Traversal.Traversal' that points
-- at a monoidal values.
--
-- This is the same operation as 'view', only infix.
--
-- @'to' f '^$' x ≡ f x@
--
-- >>> _2 ^$ (1, "hello")
-- "hello"
--
-- @
-- ('^$') ::             'Getter' s a             -> s -> a
-- ('^$') :: 'Monoid' m => 'Control.Lens.Fold.Fold' s m               -> s -> m
-- ('^$') ::             'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a         -> s -> a
-- ('^$') ::             'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a        -> s -> a
-- ('^$') :: 'Monoid' m => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s m   -> s -> m
-- @
(^$) :: Getting a s t a b -> s -> a
l ^$ s = runAccessor (l Accessor s)
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
-- >>> ("hello","world")^._2
-- "world"
--
-- >>> import Data.Complex
-- >>> ((0, 1 :+ 2), 3)^._1._2.to magnitude
-- 2.23606797749979
--
-- @
-- ('^.') ::             s -> 'Getter' s a             -> a
-- ('^.') :: 'Monoid' m => s -> 'Control.Lens.Fold.Fold' s m               -> m
-- ('^.') ::             s -> 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a         -> a
-- ('^.') ::             s -> 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a        -> a
-- ('^.') :: 'Monoid' m => s -> 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s m   -> m
-- @
(^.) :: s -> Getting a s t a b -> a
s ^. l = runAccessor (l Accessor s)
{-# INLINE (^.) #-}

-------------------------------------------------------------------------------
-- MonadState
-------------------------------------------------------------------------------

-- |
-- Use the target of a 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', or
-- 'Getter' in the current state, or use a summary of a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Traversal.Traversal' that points
-- to a monoidal value.
--
-- >>> evalState (use _1) ("hello","world")
-- "hello"
--
-- @
-- 'use' :: 'MonadState' s m             => 'Getter' s a             -> m a
-- 'use' :: ('MonadState' s m, 'Monoid' r) => 'Control.Lens.Fold.Fold' s r               -> m r
-- 'use' :: 'MonadState' s m             => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a         -> m a
-- 'use' :: 'MonadState' s m             => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a        -> m a
-- 'use' :: ('MonadState' s m, 'Monoid' r) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s r   -> m r
-- @
use :: MonadState s m => Getting a s t a b -> m a
use l = State.gets (view l)
{-# INLINE use #-}

-- |
-- Use the target of a 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso' or
-- 'Getter' in the current state, or use a summary of a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Traversal.Traversal' that
-- points to a monoidal value.
--
-- >>> evalState (uses _1 length) ("hello","world")
-- 5
--
-- @
-- 'uses' :: 'MonadState' s m             => 'Getter' s a           -> (a -> r) -> m r
-- 'uses' :: ('MonadState' s m, 'Monoid' r) => 'Control.Lens.Fold.Fold' s a             -> (a -> r) -> m r
-- 'uses' :: 'MonadState' s m             => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a      -> (a -> r) -> m r
-- 'uses' :: 'MonadState' s m             => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a       -> (a -> r) -> m r
-- 'uses' :: ('MonadState' s m, 'Monoid' r) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> (a -> r) -> m r
-- @
uses :: MonadState s m => Getting r s t a b -> (a -> r) -> m r
uses l f = State.gets (views l f)
{-# INLINE uses #-}

-- | Useful for storing getters in containers.
newtype ReifiedGetter s a = ReifyGetter { reflectGetter :: Getter s a }

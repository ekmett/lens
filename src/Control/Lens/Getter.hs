{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
#ifdef TRUSTWORTHY
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
    Getter, IndexedGetter
  , Getting, IndexedGetting
  -- * Building Getters
  , to
  -- * Combinators for Getters and Folds
  , (^.)
  , view, views, view', views'
  , use, uses, use', uses'
  -- * Indexed Getters
  -- ** Indexed Getter Combinators
  , (^@.)
  , iview, iviews
  , iuse, iuses
  -- * Implementation Details
  , Gettable
  , Accessor
  ) where

import Control.Lens.Internal
import Control.Lens.Type
import Control.Monad.Reader.Class as Reader
import Control.Monad.State        as State
import Data.Profunctor

-- $setup
-- >>> import Control.Lens
-- >>> import Data.List.Lens
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g

infixl 8 ^., ^@.

-------------------------------------------------------------------------------
-- Getters
-------------------------------------------------------------------------------

-- | Build a 'Getter' from an arbitrary Haskell function.
--
-- @'to' f . 'to' g ≡ 'to' (g . f)@
--
-- @a '^.' 'to' f ≡ f a@
--
-- >>> a ^.to f
-- f a
--
-- >>> ("hello","world")^.to snd
-- "world"
--
-- >>> 5^.to succ
-- 6
--
-- >>> (0, -5)^._2.to abs
-- 5
to :: (s -> a) -> IndexPreservingGetter s a
to f = dimap f coerce
{-# INLINE to #-}

-- |
-- When you see this in a type signature it indicates that you can
-- pass the function a 'Lens', 'Getter',
-- 'Control.Lens.Traversal.Traversal', 'Control.Lens.Fold.Fold',
-- 'Control.Lens.Prism.Prism', 'Control.Lens.Iso.Iso', or one of
-- the indexed variants, and it will just \"do the right thing\".
--
-- Most 'Getter' combinators are able to be used with both a 'Getter' or a
-- 'Control.Lens.Fold.Fold' in limited situations, to do so, they need to be
-- monomorphic in what we are going to extract with 'Const'. To be compatible
-- with 'Lens', 'Control.Lens.Traversal.Traversal' and
-- 'Control.Lens.Iso.Iso' we also restricted choices of the irrelevant @t@ and
-- @b@ parameters.
--
-- If a function accepts a @'Getting' r s t a b@, then when @r@ is a 'Monoid', then
-- you can pass a 'Control.Lens.Fold.Fold' (or
-- 'Control.Lens.Traversal.Traversal'), otherwise you can only pass this a
-- 'Getter' or 'Lens'.
--
type Getting r s t a b = (a -> Accessor r b) -> s -> Accessor r t

-- | Used to consume an 'Control.Lens.Fold.IndexedFold'.
type IndexedGetting i m s t a b = Indexed i a (Accessor m b) -> s -> Accessor m t

-------------------------------------------------------------------------------
-- Getting Values
-------------------------------------------------------------------------------

-- | View the value pointed to by a 'Getter', 'Control.Lens.Iso.Iso' or
-- 'Lens' or the result of folding over all the results of a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Traversal.Traversal' that points
-- at a monoidal values.
--
-- @'view' . 'to' ≡ 'id'@
--
-- >>> view (to f) a
-- f a
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
-- 'view' ::             'Control.Lens.Iso.Iso'' s a         -> s -> a
-- 'view' ::             'Lens'' s a        -> s -> a
-- 'view' :: 'Monoid' m => 'Control.Lens.Traversal.Traversal'' s m   -> s -> m
-- @
--
-- In a more general setting, such as when working with a monad transformer stack you can use:
--
-- @
-- 'view' :: 'MonadReader' s m             => 'Getter' s a           -> m a
-- 'view' :: ('MonadReader' s m, 'Monoid' a) => 'Control.Lens.Fold.Fold' s a             -> m a
-- 'view' :: 'MonadReader' s m             => 'Control.Lens.Iso.Iso'' s a       -> m a
-- 'view' :: 'MonadReader' s m             => 'Lens'' s a      -> m a
-- 'view' :: ('MonadReader' s m, 'Monoid' a) => 'Control.Lens.Traversal.Traversal'' s a -> m a
-- @
view :: MonadReader s m => Getting a s t a b -> m a
view l = Reader.asks (runAccessor #. l Accessor)
{-# INLINE view #-}

-- | View the value of a 'Getter', 'Control.Lens.Iso.Iso',
-- 'Lens' or the result of folding over the result of mapping
-- the targets of a 'Control.Lens.Fold.Fold' or
-- 'Control.Lens.Traversal.Traversal'.
--
-- It may be useful to think of 'views' as having these more restrictive
-- signatures:
--
-- @'views' l f ≡ 'view' (l '.' 'to' f)@
--
-- >>> views (to f) g a
-- g (f a)
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
-- 'views' ::             'Control.Lens.Iso.Iso'' s a         -> (a -> r) -> s -> r
-- 'views' ::             'Lens'' s a        -> (a -> r) -> s -> r
-- 'views' :: 'Monoid' m => 'Control.Lens.Traversal.Traversal'' s a   -> (a -> m) -> s -> m
-- @
--
-- In a more general setting, such as when working with a monad transformer stack you can use:
--
-- @
-- 'view' :: 'MonadReader' s m             => 'Getter' s a           -> m a
-- 'view' :: ('MonadReader' s m, 'Monoid' a) => 'Control.Lens.Fold.Fold' s a             -> m a
-- 'view' :: 'MonadReader' s m             => 'Control.Lens.Iso.Iso'' s a       -> m a
-- 'view' :: 'MonadReader' s m             => 'Lens'' s a      -> m a
-- 'view' :: ('MonadReader' s m, 'Monoid' a) => 'Control.Lens.Traversal.Traversal'' s a -> m a
-- @
--
-- @
-- 'views' :: 'MonadReader' s m => 'Getting' r s t a b -> (a -> r) -> m r
-- @
views :: (Profunctor p, MonadReader s m) => Overloading p (->) (Accessor r) s t a b -> p a r -> m r
views l f = Reader.asks (runAccessor #. l (rmap Accessor f))
{-# INLINE views #-}

-- | View the value pointed to by a 'Getter' or 'Lens' or the
-- result of folding over all the results of a 'Control.Lens.Fold.Fold' or
-- 'Control.Lens.Traversal.Traversal' that points at a monoidal values.
--
-- This is the same operation as 'view' with the arguments flipped.
--
-- The fixity and semantics are such that subsequent field accesses can be
-- performed with ('Prelude..')
--
-- >>> (a,b)^._2
-- b
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
-- ('^.') ::             s -> 'Control.Lens.Iso.Iso'' s a         -> a
-- ('^.') ::             s -> 'Lens'' s a        -> a
-- ('^.') :: 'Monoid' m => s -> 'Control.Lens.Traversal.Traversal'' s m   -> m
-- @
(^.) :: s -> Getting a s t a b -> a
s ^. l = runAccessor (l Accessor s)
{-# INLINE (^.) #-}

-------------------------------------------------------------------------------
-- MonadState
-------------------------------------------------------------------------------

-- |
-- Use the target of a 'Lens', 'Control.Lens.Iso.Iso', or
-- 'Getter' in the current state, or use a summary of a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Traversal.Traversal' that points
-- to a monoidal value.
--
-- >>> evalState (use _1) (a,b)
-- a
--
-- >>> evalState (use _1) ("hello","world")
-- "hello"
--
-- @
-- 'use' :: 'MonadState' s m             => 'Getter' s a             -> m a
-- 'use' :: ('MonadState' s m, 'Monoid' r) => 'Control.Lens.Fold.Fold' s r               -> m r
-- 'use' :: 'MonadState' s m             => 'Control.Lens.Iso.Iso'' s a         -> m a
-- 'use' :: 'MonadState' s m             => 'Lens'' s a        -> m a
-- 'use' :: ('MonadState' s m, 'Monoid' r) => 'Control.Lens.Traversal.Traversal'' s r   -> m r
-- @
use :: MonadState s m => Getting a s t a b -> m a
use l = State.gets (view l)
{-# INLINE use #-}

-- |
-- Use the target of a 'Lens', 'Control.Lens.Iso.Iso' or
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
-- 'uses' :: 'MonadState' s m             => 'Lens'' s a      -> (a -> r) -> m r
-- 'uses' :: 'MonadState' s m             => 'Control.Lens.Iso.Iso'' s a       -> (a -> r) -> m r
-- 'uses' :: ('MonadState' s m, 'Monoid' r) => 'Control.Lens.Traversal.Traversal'' s a -> (a -> r) -> m r
-- @
--
-- @'uses' :: 'MonadState' s m => 'Getting' r s t a b -> (a -> r) -> m r@
uses :: (Profunctor p, MonadState s m) => Overloading p (->) (Accessor r) s t a b -> p a r -> m r
uses l f = State.gets (views l f)
{-# INLINE uses #-}

------------------------------------------------------------------------------
-- Accessing State, Simplified
------------------------------------------------------------------------------

-- |
-- This is a type restricted version of 'use' that expects a 'Simple' 'Getter'.
--
-- Use the target of a 'Lens'', 'Control.Lens.Iso.Iso', or
-- 'Getter' in the current state, or use a summary of a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Traversal.Traversal' that points
-- to a monoidal value.
--
-- This use of this combinator may aid type-inference when working with lenses or traversals that
-- have non-defaultable typeclass constraints on their arguments.
--
-- >>> evalState (use' _1) (a,b)
-- a
--
--
-- >>> evalState (use' _1) ("hello","world")
-- "hello"
--
-- @
-- 'use'' :: 'MonadState' s m             => 'Getter' s a             -> m a
-- 'use'' :: ('MonadState' s m, 'Monoid' r) => 'Control.Lens.Fold.Fold' s r               -> m r
-- 'use'' :: 'MonadState' s m             => 'Control.Lens.Iso.Iso'' s a         -> m a
-- 'use'' :: 'MonadState' s m             => 'Lens'' s a        -> m a
-- 'use'' :: ('MonadState' s m, 'Monoid' r) => 'Control.Lens.Traversal.Traversal'' s r   -> m r
-- @
use' :: MonadState s m => Getting a s s a a -> m a
use' l = State.gets (view' l)
{-# INLINE use' #-}

-- |
-- This is a type restricted version of 'uses' that expects a 'Simple' 'Getter'.
--
-- Use the target of a 'Lens', 'Control.Lens.Iso.Iso' or
-- 'Getter' in the current state, or use a summary of a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Traversal.Traversal' that
-- points to a monoidal value.
--
-- >>> evalState (uses' _1 length) ("hello","world")
-- 5
--
-- @
-- 'uses'' :: 'MonadState' s m             => 'Getter' s a           -> (a -> r) -> m r
-- 'uses'' :: ('MonadState' s m, 'Monoid' r) => 'Control.Lens.Fold.Fold' s a             -> (a -> r) -> m r
-- 'uses'' :: 'MonadState' s m             => 'Lens'' s a      -> (a -> r) -> m r
-- 'uses'' :: 'MonadState' s m             => 'Control.Lens.Iso.Iso'' s a       -> (a -> r) -> m r
-- 'uses'' :: ('MonadState' s m, 'Monoid' r) => 'Control.Lens.Traversal.Traversal'' s a -> (a -> r) -> m r
-- @
--
-- @'uses'' :: 'MonadState' s m => 'Getting' r s s a a -> (a -> r) -> m r@
uses' :: (Profunctor p, MonadState s m) => Overloading' p (->) (Accessor r) s a -> p a r -> m r
uses' l f = State.gets (views' l f)
{-# INLINE uses' #-}

------------------------------------------------------------------------------
-- Viewing, Simplified
------------------------------------------------------------------------------

-- | This is a type restricted version of 'view' that expects a 'Simple' 'Getter'.
--
-- View the value pointed to by a 'Getter', 'Control.Lens.Iso.Iso' or
-- 'Lens' or the result of folding over all the results of a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Traversal.Traversal' that points
-- at a monoidal values.
--
-- @'view'' . 'to' ≡ 'id'@
--
-- >>> view' (to f) a
-- f a
--
-- >>> view' _2 (1,"hello")
-- "hello"
--
-- >>> view' (to succ) 5
-- 6
--
-- >>> view' (_2._1) ("hello",("world","!!!"))
-- "world"
--
-- As 'view'' is commonly used to access the target of a 'Getter' or obtain a monoidal summary of the targets of a 'Fold',
-- It may be useful to think of it as having one of these more restrictive signatures:
--
-- @
-- 'view'' ::             'Getter' s a             -> s -> a
-- 'view'' :: 'Monoid' m => 'Control.Lens.Fold.Fold' s m               -> s -> m
-- 'view'' ::             'Control.Lens.Iso.Iso'' s a         -> s -> a
-- 'view'' ::             'Lens'' s a        -> s -> a
-- 'view'' :: 'Monoid' m => 'Control.Lens.Traversal.Traversal'' s m   -> s -> m
-- @
--
-- In a more general setting, such as when working with a monad transformer stack you can use:
--
-- @
-- 'view'' :: 'MonadReader' s m             => 'Getter' s a           -> m a
-- 'view'' :: ('MonadReader' s m, 'Monoid' a) => 'Control.Lens.Fold.Fold' s a             -> m a
-- 'view'' :: 'MonadReader' s m             => 'Control.Lens.Iso.Iso'' s a       -> m a
-- 'view'' :: 'MonadReader' s m             => 'Lens'' s a      -> m a
-- 'view'' :: ('MonadReader' s m, 'Monoid' a) => 'Control.Lens.Traversal.Traversal'' s a -> m a
-- @
view' :: MonadReader s m => Getting a s s a a -> m a
view' l = Reader.asks (runAccessor #. l Accessor)
{-# INLINE view' #-}

-- | This is a type restricted version of 'views' that expects a 'Simple' 'Getter'.
--
-- View the value of a 'Getter', 'Control.Lens.Iso.Iso',
-- 'Lens' or the result of folding over the result of mapping
-- the targets of a 'Control.Lens.Fold.Fold' or
-- 'Control.Lens.Traversal.Traversal'.
--
-- It may be useful to think of 'perviews' as having these more restrictive
-- signatures:
--
-- @'views'' l f ≡ 'view'' (l '.' 'to' f)@
--
-- >>> views' _2 length (1,"hello")
-- 5
--
-- As 'views'' is commonly used to access the target of a 'Getter' or obtain a monoidal summary of the targets of a 'Fold',
-- It may be useful to think of it as having one of these more restrictive signatures:
--
-- @
-- 'views'' ::             'Getter' s a             -> (a -> r) -> s -> r
-- 'views'' :: 'Monoid' m => 'Control.Lens.Fold.Fold' s a               -> (a -> m) -> s -> m
-- 'views'' ::             'Control.Lens.Iso.Iso'' s a         -> (a -> r) -> s -> r
-- 'views'' ::             'Lens'' s a        -> (a -> r) -> s -> r
-- 'views'' :: 'Monoid' m => 'Control.Lens.Traversal.Traversal'' s a   -> (a -> m) -> s -> m
-- @
--
-- In a more general setting, such as when working with a monad transformer stack you can use:
--
-- @
-- 'views'' :: 'MonadReader' s m             => 'Getter' s a           -> (a -> r) -> m r
-- 'views'' :: ('MonadReader' s m, 'Monoid' a) => 'Control.Lens.Fold.Fold' s a             -> (a -> r) -> m r
-- 'views'' :: 'MonadReader' s m             => 'Control.Lens.Iso.Iso'' s a       -> (a -> r) -> m r
-- 'views'' :: 'MonadReader' s m             => 'Lens'' s a      -> (a -> r) -> m r
-- 'views'' :: ('MonadReader' s m, 'Monoid' a) => 'Control.Lens.Traversal.Traversal'' s a -> (a -> r) -> m r
-- @
--
-- @'views'' :: 'MonadReader' s m => 'Getting' r s s a a -> (a -> r) -> m r@
views' :: (Profunctor p, MonadReader s m) => Overloading' p (->) (Accessor r) s a -> p a r -> m r
views' l f = Reader.asks (runAccessor #. l (rmap Accessor f))
{-# INLINE views' #-}

------------------------------------------------------------------------------
-- Indexed Getters
------------------------------------------------------------------------------

-- | View the index and value of an 'IndexedGetter' into the current environment as a pair.
--
-- When applied to an 'IndexedFold' the result will most likely be a nonsensical monoidal summary of
-- the indices tupled with a monoidal summary of the values and probably not whatever it is you wanted.
iview :: MonadReader s m => IndexedGetting i (i,a) s t a b -> m (i,a)
iview l = asks (runAccessor #. l (Indexed $ \i -> Accessor #. (,) i))
{-# INLINE iview #-}

-- | View a function of the index and value of an 'IndexedGetter' into the current environment
--
-- When applied to an 'IndexedFold' the result will be a monoidal summary instead of a single answer.
--
-- @'iviews' ≡ 'Control.Lens.Fold.ifoldMapOf'@
iviews :: MonadReader s m => IndexedGetting i r s t a b -> (i -> a -> r) -> m r
iviews l = views l .# Indexed
{-# INLINE iviews #-}

-- | Use the index and value of an 'IndexedGetter' into the current state as a pair.
--
-- When applied to an 'IndexedFold' the result will most likely be a nonsensical monoidal summary of
-- the indices tupled with a monoidal summary of the values and probably not whatever it is you wanted.
iuse :: MonadState s m => IndexedGetting i (i,a) s t a b -> m (i,a)
iuse l = gets (runAccessor #. l (Indexed $ \i -> Accessor #. (,) i))
{-# INLINE iuse #-}

-- | Use a function of the index and value of an 'IndexedGetter' into the current state.
--
-- When applied to an 'IndexedFold' the result will be a monoidal summary instead of a single answer.
iuses :: MonadState s m => IndexedGetting i r s t a b -> (i -> a -> r) -> m r
iuses l = uses l .# Indexed
{-# INLINE iuses #-}

-- | View the value pointed to by a 'Getter' or 'Lens'.
--
-- This is the same operation as 'iview' with the arguments flipped.
--
-- The fixity and semantics are such that subsequent field accesses can be
-- performed with ('Prelude..')
--
-- >>> (a,b,c,d)^@._2
-- (1,b)
--
-- >>> ("hello","world","!!!")^@._2
-- (1,"world")
--
-- @
-- ('^@.') :: s -> 'IndexedGetter' i s a             -> (i, a)
-- ('^@.') :: s -> 'IndexedLens'' i s a        -> (i, a)
-- @
--
-- The result probably doesn't have much meaning when applied to an 'IndexedFold'.
(^@.) :: s -> IndexedGetting i (i, a) s t a b -> (i, a)
s ^@. l = runAccessor $ l (Indexed $ \i -> Accessor #. (,) i) s
{-# INLINE (^@.) #-}

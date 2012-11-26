{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x,y,z) 1
#endif

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Type
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- A @'Lens' s t a b@ is a purely functional reference.
--
-- While a 'Control.Lens.Traversal.Traversal' could be used for
-- 'Control.Lens.Getter.Getting' like a valid 'Control.Lens.Fold.Fold',
-- it wasn't a valid 'Control.Lens.Getter.Getter' as 'Applicative' wasn't a superclass of
-- 'Control.Lens.Getter.Gettable'.
--
-- 'Functor', however is the superclass of both.
--
-- @type 'Lens' s t a b = forall f. 'Functor' f => (a -> f b) -> s -> f t@
--
-- Every 'Lens' is a valid 'Control.Lens.Setter.Setter'.
--
-- Every 'Lens' can be used for 'Control.Lens.Getter.Getting' like a
-- 'Control.Lens.Fold.Fold' that doesn't use the 'Applicative' or
-- 'Control.Lens.Getter.Gettable'.
--
-- Every 'Lens' is a valid 'Control.Lens.Traversal.Traversal' that only uses
-- the 'Functor' part of the 'Applicative' it is supplied.
--
-- Every 'Lens' can be used for 'Control.Lens.Getter.Getting' like a valid
-- 'Control.Lens.Getter.Getter', since 'Functor' is a superclass of 'Control.Lens.Getter.Gettable'
--
-- Since every 'Lens' can be used for 'Control.Lens.Getter.Getting' like a
-- valid 'Control.Lens.Getter.Getter' it follows that it must view exactly one element in the
-- structure.
--
-- The lens laws follow from this property and the desire for it to act like
-- a 'Data.Traversable.Traversable' when used as a
-- 'Control.Lens.Traversal.Traversal'.
-------------------------------------------------------------------------------
module Control.Lens.Type
  (
  -- * Lenses
    Lens
  , Simple

  , lens
  , (%%~)
  , (%%=)

  -- * Lateral Composition
  , choosing
  , chosen
  , alongside

  -- * Setting Functionally with Passthrough
  , (<%~), (<+~), (<-~), (<*~), (<//~)
  , (<^~), (<^^~), (<**~)
  , (<||~), (<&&~)
  , (<<%~), (<<.~)

  -- * Setting State with Passthrough
  , (<%=), (<+=), (<-=), (<*=), (<//=)
  , (<^=), (<^^=), (<**=)
  , (<||=), (<&&=)
  , (<<%=), (<<.=)
  , (<<~)

  -- * Cloning Lenses
  , cloneLens
  , ReifiedLens(..)

  -- * Simplified and In-Progress
  , LensLike
  , Overloaded
  , SimpleLens
  , SimpleLensLike
  , SimpleOverloaded
  , SimpleReifiedLens

  -- * Exposed Implementation Details
  , Context(..)
  ) where

import Control.Applicative              as Applicative
import Control.Lens.Internal
import Control.Monad.State              as State

-- $setup
-- >>> import Control.Lens

infixr 4 %%~
infix  4 %%=
infixr 4 <+~, <*~, <-~, <//~, <^~, <^^~, <**~, <&&~, <||~, <%~, <<%~, <<.~
infix  4 <+=, <*=, <-=, <//=, <^=, <^^=, <**=, <&&=, <||=, <%=, <<%=, <<.=
infixr 2 <<~

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

-- | A 'Lens' is actually a lens family as described in
-- <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- With great power comes great responsibility and a 'Lens' is subject to the
-- three common sense lens laws:
--
-- 1) You get back what you put in:
--
-- @'Control.Lens.Getter.view' l ('Control.Lens.Setter.set' l b a)  ≡ b@
--
-- 2) Putting back what you got doesn't change anything:
--
-- @'Control.Lens.Setter.set' l ('Control.Lens.Getter.view' l a) a  ≡ a@
--
-- 3) Setting twice is the same as setting once:
--
-- @'Control.Lens.Setter.set' l c ('Control.Lens.Setter.set' l b a) ≡ 'Control.Lens.Setter.set' l c a@
--
-- These laws are strong enough that the 4 type parameters of a 'Lens' cannot
-- vary fully independently. For more on how they interact, read the \"Why is
-- it a Lens Family?\" section of
-- <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- Every 'Lens' can be used directly as a 'Control.Lens.Setter.Setter' or
-- 'Control.Lens.Traversal.Traversal'.
--
-- You can also use a 'Lens' for 'Control.Lens.Getter.Getting' as if it were a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Getter.Getter'.
--
-- Since every lens is a valid 'Control.Lens.Traversal.Traversal', the
-- traversal laws are required of any lenses you create:
--
-- @
-- l 'pure' ≡ 'pure'
-- 'fmap' (l f) '.' l g ≡ 'Data.Functor.Compose.getCompose' '.' l ('Data.Functor.Compose.Compose' '.' 'fmap' f '.' g)
-- @
--
-- @type 'Lens' s t a b = forall f. 'Functor' f => 'LensLike' f s t a b@
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | A 'Simple' 'Lens', 'Simple' 'Control.Lens.Traversal.Traversal', ... can
-- be used instead of a 'Lens','Control.Lens.Traversal.Traversal', ...
-- whenever the type variables don't change upon setting a value.
--
-- @
-- 'Data.Complex.Lens.imaginary' :: 'Simple' 'Lens' ('Data.Complex.Complex' a) a
-- 'Data.List.Lens.traverseHead' :: 'Simple' 'Control.Lens.Lens.Traversal' [a] a
-- @
--
-- Note: To use this alias in your own code with @'LensLike' f@ or
-- 'Control.Lens.Setter.Setter', you may have to turn on @LiberalTypeSynonyms@.
type Simple f s a = f s s a a

-- | @type 'SimpleLens' = 'Simple' 'Lens'@
type SimpleLens s a = Lens s s a a

-- | @type 'SimpleLensLike' f = 'Simple' ('LensLike' f)@
type SimpleLensLike f s a = LensLike f s s a a

--------------------------
-- Constructing Lenses
--------------------------

-- | Build a 'Lens' from a getter and a setter.
--
-- @'lens' :: 'Functor' f => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t@
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

-------------------------------------------------------------------------------
-- LensLike
-------------------------------------------------------------------------------

-- |
-- Many combinators that accept a 'Lens' can also accept a
-- 'Control.Lens.Traversal.Traversal' in limited situations.
--
-- They do so by specializing the type of 'Functor' that they require of the
-- caller.
--
-- If a function accepts a @'LensLike' f s t a b@ for some 'Functor' @f@,
-- then they may be passed a 'Lens'.
--
-- Further, if @f@ is an 'Applicative', they may also be passed a
-- 'Control.Lens.Traversal.Traversal'.
type LensLike f s t a b = (a -> f b) -> s -> f t

-- | ('%%~') can be used in one of two scenarios:
--
-- When applied to a 'Lens', it can edit the target of the 'Lens' in a
-- structure, extracting a functorial result.
--
-- When applied to a 'Control.Lens.Traversal.Traversal', it can edit the
-- targets of the 'Traversals', extracting an applicative summary of its
-- actions.
--
-- For all that the definition of this combinator is just:
--
-- @('%%~') ≡ 'id'@
--
-- @
-- ('%%~') :: 'Functor' f =>     'Control.Lens.Iso.Iso' s t a b       -> (a -> f b) -> s -> f t
-- ('%%~') :: 'Functor' f =>     'Lens' s t a b      -> (a -> f b) -> s -> f t
-- ('%%~') :: 'Applicative' f => 'Control.Lens.Traversal.Traversal' s t a b -> (a -> f b) -> s -> f t
-- @
--
-- It may be beneficial to think about it as if it had these even more
-- restrictive types, however:
--
-- When applied to a 'Control.Lens.Traversal.Traversal', it can edit the
-- targets of the 'Traversals', extracting a supplemental monoidal summary
-- of its actions, by choosing @f = ((,) m)@
--
-- @
-- ('%%~') ::             'Control.Lens.Iso.Iso' s t a b       -> (a -> (r, b)) -> s -> (r, t)
-- ('%%~') ::             'Lens' s t a b      -> (a -> (r, b)) -> s -> (r, t)
-- ('%%~') :: 'Monoid' m => 'Control.Lens.Traversal.Traversal' s t a b -> (a -> (m, b)) -> s -> (m, t)
-- @
(%%~) :: LensLike f s t a b -> (a -> f b) -> s -> f t
(%%~) = id
{-# INLINE (%%~) #-}

-- | Modify the target of a 'Lens' in the current state returning some extra
-- information of type @r@ or modify all targets of a
-- 'Control.Lens.Traversal.Traversal' in the current state, extracting extra
-- information of type @r@ and return a monoidal summary of the changes.
--
-- @('%%=') ≡ ('state' '.')@
--
-- It may be useful to think of ('%%='), instead, as having either of the
-- following more restricted type signatures:
--
-- @
-- ('%%=') :: 'MonadState' s m             => 'Control.Lens.Iso.Iso' s s a b       -> (a -> (r, b)) -> m r
-- ('%%=') :: 'MonadState' s m             => 'Lens' s s a b      -> (a -> (r, b)) -> m r
-- ('%%=') :: ('MonadState' s m, 'Monoid' r) => 'Control.Lens.Traversal.Traversal' s s a b -> (a -> (r, b)) -> m r
-- @
(%%=) :: MonadState s m => LensLike ((,) r) s s a b -> (a -> (r, b)) -> m r
#if MIN_VERSION_mtl(2,1,1)
l %%= f = State.state (l f)
#else
l %%= f = do
  (r, s) <- State.gets (l f)
  State.put s
  return r
#endif
{-# INLINE (%%=) #-}

-------------------------------------------------------------------------------
-- Common Lenses
-------------------------------------------------------------------------------

-- | Merge two lenses, getters, setters, folds or traversals.
--
-- @'chosen' ≡ 'choosing' 'id' 'id'@
--
-- @
-- 'choosing' :: 'Control.Lens.Getter.Getter' s a           -> 'Control.Lens.Getter.Getter' s' a           -> 'Control.Lens.Getter.Getter' ('Either' s s') a
-- 'choosing' :: 'Control.Lens.Fold.Fold' s a             -> 'Control.Lens.Fold.Fold' s' a             -> 'Control.Lens.Fold.Fold' ('Either' s s') a
-- 'choosing' :: 'Simple' 'Lens' s a      -> 'Simple' 'Lens' s' a      -> 'Simple' 'Lens' ('Either' s s') a
-- 'choosing' :: 'Simple' 'Control.Lens.Traversal.Traversal' s a -> 'Simple' 'Control.Lens.Traversal.Traversal' s' a -> 'Simple' 'Control.Lens.Traversal.Traversal' ('Either' s s') a
-- 'choosing' :: 'Simple' 'Control.Lens.Setter.Setter' s a    -> 'Simple' 'Control.Lens.Setter.Setter' s' a    -> 'Simple' 'Control.Lens.Setter.Setter' ('Either' s s') a
-- @
choosing :: Functor f
       => LensLike f s t a a
       -> LensLike f s' t' a a
       -> LensLike f (Either s s') (Either t t') a a
choosing l _ f (Left a)   = Left <$> l f a
choosing _ r f (Right a') = Right <$> r f a'
{-# INLINE choosing #-}

-- | This is a 'Lens' that updates either side of an 'Either', where both sides have the same type.
--
-- @'chosen' ≡ 'choosing' 'id' 'id'@
--
-- >>> Left 12^.chosen
-- 12
-- >>> Right "hello"^.chosen
-- "hello"
-- >>> chosen *~ 10 $ Right 2
-- Right 20
chosen :: Lens (Either a a) (Either b b) a b
chosen f (Left a) = Left <$> f a
chosen f (Right a) = Right <$> f a
{-# INLINE chosen #-}

-- | 'alongside' makes a 'Lens' from two other lenses.
--
-- @'alongside' :: 'Lens' s t a b -> 'Lens' s' t' a' b' -> 'Lens' (s,s') (t,t') (a,a') (b,b')@
alongside :: LensLike (Context a b) s t a b
           -> LensLike (Context a' b')  s' t' a' b'
           -> Lens (s,s') (t,t') (a,a') (b,b')
alongside l r f (s, s') = case l (Context id) s of
  Context bt a -> case r (Context id) s' of
    Context bt' a' -> (\(b,b') -> (bt b, bt' b')) <$> f (a,a')
{-# INLINE alongside #-}

-------------------------------------------------------------------------------
-- Cloning Lenses
-------------------------------------------------------------------------------

-- |
-- Cloning a 'Lens' is one way to make sure you aren't given
-- something weaker, such as a 'Control.Lens.Traversal.Traversal' and can be
-- used as a way to pass around lenses that have to be monomorphic in @f@.
--
-- Note: This only accepts a proper 'Lens'.
--
-- /\"Costate Comonad Coalgebra is equivalent of Java's member variable update technology for Haskell\"/ -- \@PLT_Borat on Twitter
cloneLens :: Functor f
  => LensLike (Context a b) s t a b
  -> (a -> f b) -> s -> f t
cloneLens f afb s = case f (Context id) s of
  Context bt a -> bt <$> afb a
{-# INLINE cloneLens #-}

-------------------------------------------------------------------------------
-- Overloading function application
-------------------------------------------------------------------------------

-- | @type 'LensLike' f s t a b = 'Overloaded' (->) f s t a b@
type Overloaded k f s t a b = k (a -> f b) (s -> f t)

-- | @type 'SimpleOverloaded' k f s a = 'Simple' ('Overloaded' k f) s a@
type SimpleOverloaded k f s a = Overloaded k f s s a a

-------------------------------------------------------------------------------
-- Setting and Remembering
-------------------------------------------------------------------------------

-- | Modify the target of a 'Lens' and return the result
--
-- When you do not need the result of the addition, ('Control.Lens.Setter.%~') is more flexible.
--
-- @
-- ('<%~') ::             'Lens' s t a b      -> (a -> b) -> s -> (b, t)
-- ('<%~') ::             'Control.Lens.Iso.Iso' s t a b       -> (a -> b) -> s -> (b, t)
-- ('<%~') :: 'Monoid' b => 'Control.Lens.Traversal.Traversal' s t a b -> (a -> b) -> s -> (b, t)
-- @
(<%~) :: LensLike ((,)b) s t a b -> (a -> b) -> s -> (b, t)
l <%~ f = l $ \s -> let t = f s in (t, t)
{-# INLINE (<%~) #-}

-- | Increment the target of a numerically valued 'Lens' and return the result
--
-- When you do not need the result of the addition, ('Control.Lens.Setter.+~') is more flexible.
--
-- @
-- ('<+~') :: 'Num' a => 'Simple' 'Lens' s a -> a -> s -> (a, s)
-- ('<+~') :: 'Num' a => 'Simple' 'Control.Lens.Iso.Iso' s a  -> a -> s -> (a, s)
-- @
(<+~) :: Num a => LensLike ((,)a) s t a a -> a -> s -> (a, t)
l <+~ a = l <%~ (+ a)
{-# INLINE (<+~) #-}

-- | Decrement the target of a numerically valued 'Lens' and return the result
--
-- When you do not need the result of the subtraction, ('Control.Lens.Setter.-~') is more flexible.
--
-- @
-- ('<-~') :: 'Num' a => 'Simple' 'Lens' s a -> a -> s -> (a, s)
-- ('<-~') :: 'Num' a => 'Simple' 'Control.Lens.Iso.Iso' s a  -> a -> s -> (a, s)
-- @
(<-~) :: Num a => LensLike ((,)a) s t a a -> a -> s -> (a, t)
l <-~ a = l <%~ subtract a
{-# INLINE (<-~) #-}

-- | Multiply the target of a numerically valued 'Lens' and return the result
--
-- When you do not need the result of the multiplication, ('Control.Lens.Setter.*~') is more
-- flexible.
--
-- @
-- ('<*~') :: 'Num' b => 'Simple' 'Lens' s a -> a -> a -> (s, a)
-- ('<*~') :: 'Num' b => 'Simple' 'Control.Lens.Iso.Iso' s a -> a -> a -> (s, a))
-- @
(<*~) :: Num a => LensLike ((,)a) s t a a -> a -> s -> (a, t)
l <*~ a = l <%~ (* a)
{-# INLINE (<*~) #-}

-- | Divide the target of a fractionally valued 'Lens' and return the result.
--
-- When you do not need the result of the division, ('Control.Lens.Setter.//~') is more flexible.
--
-- @
-- ('<//~') :: 'Fractional' b => 'Simple' 'Lens' s a -> a -> a -> (s, a)
-- ('<//~') :: 'Fractional' b => 'Simple' 'Control.Lens.Iso.Iso' s a -> a -> a -> (s, a))
-- @
(<//~) :: Fractional a => LensLike ((,)a) s t a a -> a -> s -> (a, t)
l <//~ a = l <%~ (/ a)
{-# INLINE (<//~) #-}

-- | Raise the target of a numerically valued 'Lens' to a non-negative
-- 'Integral' power and return the result
--
-- When you do not need the result of the division, ('Control.Lens.Setter.^~') is more flexible.
--
-- @
-- ('<^~') :: ('Num' b, 'Integral' e) => 'Simple' 'Lens' s a -> e -> a -> (a, s)
-- ('<^~') :: ('Num' b, 'Integral' e) => 'Simple' 'Control.Lens.Iso.Iso' s a -> e -> a -> (a, s)
-- @
(<^~) :: (Num a, Integral e) => LensLike ((,)a) s t a a -> e -> s -> (a, t)
l <^~ e = l <%~ (^ e)
{-# INLINE (<^~) #-}

-- | Raise the target of a fractionally valued 'Lens' to an 'Integral' power
-- and return the result.
--
-- When you do not need the result of the division, ('Control.Lens.Setter.^^~') is more flexible.
--
-- @
-- ('<^^~') :: ('Fractional' b, 'Integral' e) => 'Simple' 'Lens' s a -> e -> a -> (a, s)
-- ('<^^~') :: ('Fractional' b, 'Integral' e) => 'Simple' 'Control.Lens.Iso.Iso' s a -> e -> a -> (a, s)
-- @
(<^^~) :: (Fractional a, Integral e) => LensLike ((,)a) s t a a -> e -> s -> (a, t)
l <^^~ e = l <%~ (^^ e)
{-# INLINE (<^^~) #-}

-- | Raise the target of a floating-point valued 'Lens' to an arbitrary power
-- and return the result.
--
-- When you do not need the result of the division, ('Control.Lens.Setter.**~') is more flexible.
--
-- @
-- ('<**~') :: 'Floating' a => 'Simple' 'Lens' s a -> a -> s -> (a, s)
-- ('<**~') :: 'Floating' a => 'Simple' 'Control.Lens.Iso.Iso' s a  -> a -> s -> (a, s)
-- @
(<**~) :: Floating a => LensLike ((,)a) s t a a -> a -> s -> (a, t)
l <**~ a = l <%~ (** a)
{-# INLINE (<**~) #-}

-- | Logically '||' a Boolean valued 'Lens' and return the result
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.||~') is more flexible.
--
-- @
-- ('<||~') :: 'Simple' 'Lens' s 'Bool' -> 'Bool' -> s -> ('Bool', s)
-- ('<||~') :: 'Simple' 'Control.Lens.Iso.Iso' s 'Bool'  -> 'Bool' -> s -> ('Bool', s)
-- @
(<||~) :: LensLike ((,)Bool) s t Bool Bool -> Bool -> s -> (Bool, t)
l <||~ b = l <%~ (|| b)
{-# INLINE (<||~) #-}

-- | Logically '&&' a Boolean valued 'Lens' and return the result
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.&&~') is more flexible.
--
-- @
-- ('<&&~') :: 'Simple' 'Lens' s 'Bool' -> 'Bool' -> s -> ('Bool', s)
-- ('<&&~') :: 'Simple' 'Control.Lens.Iso.Iso' s 'Bool'  -> 'Bool' -> s -> ('Bool', s)
-- @
(<&&~) :: LensLike ((,)Bool) s t Bool Bool -> Bool -> s -> (Bool, t)
l <&&~ b = l <%~ (&& b)
{-# INLINE (<&&~) #-}

-- | Modify the target of a 'Lens', but return the old value.
--
-- When you do not need the result of the addition, ('Control.Lens.Setter.%~') is more flexible.
--
-- @
-- ('<<%~') ::             'Lens' s t a b      -> (a -> b) -> s -> (b, t)
-- ('<<%~') ::             'Control.Lens.Iso.Iso' s t a b       -> (a -> b) -> s -> (b, t)
-- ('<<%~') :: 'Monoid' b => 'Control.Lens.Traversal.Traversal' s t a b -> (a -> b) -> s -> (b, t)
-- @
(<<%~) :: LensLike ((,)a) s t a b -> (a -> b) -> s -> (a, t)
l <<%~ f = l $ \a -> (a, f a)
{-# INLINE (<<%~) #-}

-- | Modify the target of a 'Lens', but return the old value.
--
-- When you do not need the old value, ('Control.Lens.Setter.%~') is more flexible.
--
-- @
-- ('<<%~') ::             'Lens' s t a b      -> b -> s -> (a, t)
-- ('<<%~') ::             'Control.Lens.Iso.Iso' s t a b       -> b -> s -> (a, t)
-- ('<<%~') :: 'Monoid' b => 'Control.Lens.Traversal.Traversal' s t a b -> b -> s -> (a, t)
-- @
(<<.~) :: LensLike ((,)a) s t a b -> b -> s -> (a, t)
l <<.~ b = l $ \a -> (a, b)
{-# INLINE (<<.~) #-}

-------------------------------------------------------------------------------
-- Setting and Remembering State
-------------------------------------------------------------------------------

-- | Modify the target of a 'Lens' into your monad's state by a user supplied
-- function and return the result.
--
-- When applied to a 'Control.Lens.Traversal.Traversal', it this will return a monoidal summary of all of the intermediate
-- results.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.%=') is more flexible.
--
-- @
-- ('<%=') :: 'MonadState' s m             => 'Simple' 'Lens' s a     -> (a -> a) -> m a
-- ('<%=') :: 'MonadState' s m             => 'Simple' 'Control.Lens.Iso.Iso' s a      -> (a -> a) -> m a
-- ('<%=') :: ('MonadState' s m, 'Monoid' a) => 'Simple' 'Traveral' s a -> (a -> a) -> m a
-- @
(<%=) :: MonadState s m => LensLike ((,)b) s s a b -> (a -> b) -> m b
l <%= f = l %%= \a -> let b = f a in (b,b)
{-# INLINE (<%=) #-}


-- | Add to the target of a numerically valued 'Lens' into your monad's state
-- and return the result.
--
-- When you do not need the result of the addition, ('Control.Lens.Setter.+=') is more
-- flexible.
--
-- @
-- ('<+=') :: ('MonadState' s m, 'Num' a) => 'Simple' 'Lens' s a -> a -> m a
-- ('<+=') :: ('MonadState' s m, 'Num' a) => 'Simple' 'Control.Lens.Iso.Iso' s a -> a -> m a
-- @
(<+=) :: (MonadState s m, Num a) => SimpleLensLike ((,)a) s a -> a -> m a
l <+= a = l <%= (+ a)
{-# INLINE (<+=) #-}

-- | Subtract from the target of a numerically valued 'Lens' into your monad's
-- state and return the result.
--
-- When you do not need the result of the subtraction, ('Control.Lens.Setter.-=') is more
-- flexible.
--
-- @
-- ('<-=') :: ('MonadState' s m, 'Num' a) => 'Simple' 'Lens' s a -> a -> m a
-- ('<-=') :: ('MonadState' s m, 'Num' a) => 'Simple' 'Control.Lens.Iso.Iso' s a -> a -> m a
-- @
(<-=) :: (MonadState s m, Num a) => SimpleLensLike ((,)a) s a -> a -> m a
l <-= a = l <%= subtract a
{-# INLINE (<-=) #-}

-- | Multiply the target of a numerically valued 'Lens' into your monad's
-- state and return the result.
--
-- When you do not need the result of the multiplication, ('Control.Lens.Setter.*=') is more
-- flexible.
--
-- @
-- ('<*=') :: ('MonadState' s m, 'Num' a) => 'Simple' 'Lens' s a -> a -> m a
-- ('<*=') :: ('MonadState' s m, 'Num' a) => 'Simple' 'Control.Lens.Iso.Iso' s a -> a -> m a
-- @
(<*=) :: (MonadState s m, Num a) => SimpleLensLike ((,)a) s a -> a -> m a
l <*= a = l <%= (* a)
{-# INLINE (<*=) #-}

-- | Divide the target of a fractionally valued 'Lens' into your monad's state
-- and return the result.
--
-- When you do not need the result of the division, ('Control.Lens.Setter.//=') is more flexible.
--
-- @
-- ('<//=') :: ('MonadState' s m, 'Fractional' a) => 'Simple' 'Lens' s a -> a -> m a
-- ('<//=') :: ('MonadState' s m, 'Fractional' a) => 'Simple' 'Control.Lens.Iso.Iso' s a -> a -> m a
-- @
(<//=) :: (MonadState s m, Fractional a) => SimpleLensLike ((,)a) s a -> a -> m a
l <//= a = l <%= (/ a)
{-# INLINE (<//=) #-}

-- | Raise the target of a numerically valued 'Lens' into your monad's state
-- to a non-negative 'Integral' power and return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.**=') is more flexible.
--
-- @
-- ('<^=') :: ('MonadState' s m, 'Num' a, 'Integral' e) => 'Simple' 'Lens' s a -> e -> m a
-- ('<^=') :: ('MonadState' s m, 'Num' a, 'Integral' e) => 'Simple' 'Control.Lens.Iso.Iso' s a -> e -> m a
-- @
(<^=) :: (MonadState s m, Num a, Integral e) => SimpleLensLike ((,)a) s a -> e -> m a
l <^= e = l <%= (^ e)
{-# INLINE (<^=) #-}

-- | Raise the target of a fractionally valued 'Lens' into your monad's state
-- to an 'Integral' power and return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.^^=') is more flexible.
--
-- @
-- ('<^^=') :: ('MonadState' s m, 'Fractional' b, 'Integral' e) => 'Simple' 'Lens' s a -> e -> m a
-- ('<^^=') :: ('MonadState' s m, 'Fractional' b, 'Integral' e) => 'Simple' 'Control.Lens.Iso.Iso' s a  -> e -> m a
-- @
(<^^=) :: (MonadState s m, Fractional a, Integral e) => SimpleLensLike ((,)a) s a -> e -> m a
l <^^= e = l <%= (^^ e)
{-# INLINE (<^^=) #-}

-- | Raise the target of a floating-point valued 'Lens' into your monad's
-- state to an arbitrary power and return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.**=') is more flexible.
--
-- @
-- ('<**=') :: ('MonadState' s m, 'Floating' a) => 'Simple' 'Lens' s a -> a -> m a
-- ('<**=') :: ('MonadState' s m, 'Floating' a) => 'Simple' 'Control.Lens.Iso.Iso' s a -> a -> m a
-- @
(<**=) :: (MonadState s m, Floating a) => SimpleLensLike ((,)a) s a -> a -> m a
l <**= a = l <%= (** a)
{-# INLINE (<**=) #-}

-- | Logically '||' a Boolean valued 'Lens' into your monad's state and return
-- the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.||=') is more flexible.
--
-- @
-- ('<||=') :: 'MonadState' s m => 'Simple' 'Lens' s 'Bool' -> 'Bool' -> m 'Bool'
-- ('<||=') :: 'MonadState' s m => 'Simple' 'Control.Lens.Iso.Iso' s 'Bool'  -> 'Bool' -> m 'Bool'
-- @
(<||=) :: MonadState s m => SimpleLensLike ((,)Bool) s Bool -> Bool -> m Bool
l <||= b = l <%= (|| b)
{-# INLINE (<||=) #-}

-- | Logically '&&' a Boolean valued 'Lens' into your monad's state and return
-- the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.&&=') is more flexible.
--
-- @
-- ('<&&=') :: 'MonadState' s m => 'Simple' 'Lens' s 'Bool' -> 'Bool' -> m 'Bool'
-- ('<&&=') :: 'MonadState' s m => 'Simple' 'Control.Lens.Iso.Iso' s 'Bool'  -> 'Bool' -> m 'Bool'
-- @
(<&&=) :: MonadState s m => SimpleLensLike ((,)Bool) s Bool -> Bool -> m Bool
l <&&= b = l <%= (&& b)
{-# INLINE (<&&=) #-}

-- | Modify the target of a 'Lens' into your monad's state by a user supplied
-- function and return the /old/ value that was replaced.
--
-- When applied to a 'Control.Lens.Traversal.Traversal', it this will return a monoidal summary of all of the old values
-- present.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.%=') is more flexible.
--
-- @
-- ('<<%=') :: 'MonadState' s m             => 'Simple' 'Lens' s a     -> (a -> a) -> m a
-- ('<<%=') :: 'MonadState' s m             => 'Simple' 'Control.Lens.Iso.Iso' s a      -> (a -> a) -> m a
-- ('<<%=') :: ('MonadState' s m, 'Monoid' b) => 'Simple' 'Traveral' s a -> (a -> a) -> m a
-- @
(<<%=) :: MonadState s m => LensLike ((,)a) s s a b -> (a -> b) -> m a
l <<%= f = l %%= \a -> (a, f a)
{-# INLINE (<<%=) #-}

-- | Modify the target of a 'Lens' into your monad's state by a user supplied
-- function and return the /old/ value that was replaced.
--
-- When applied to a 'Control.Lens.Traversal.Traversal', it this will return a monoidal summary of all of the old values
-- present.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.%=') is more flexible.
--
-- @
-- ('<<%=') :: 'MonadState' s m             => 'Simple' 'Lens' s a     -> (a -> a) -> m a
-- ('<<%=') :: 'MonadState' s m             => 'Simple' 'Control.Lens.Iso.Iso' s a      -> (a -> a) -> m a
-- ('<<%=') :: ('MonadState' s m, 'Monoid' t) => 'Simple' 'Traveral' s a -> (a -> a) -> m a
-- @
(<<.=) :: MonadState s m => LensLike ((,)a) s s a b -> b -> m a
l <<.= b = l %%= \a -> (a,b)
{-# INLINE (<<.=) #-}

-- | Run a monadic action, and set the target of 'Lens' to its result.
--
-- @
-- ('<<~') :: 'MonadState' s m => 'Control.Lens.Iso.Iso' s s a b   -> m b -> m b
-- ('<<~') :: 'MonadState' s m => 'Control.Lens.Type.Lens' s s a b  -> m b -> m b
-- @
--
-- NB: This is limited to taking an actual 'Lens' than admitting a 'Control.Lens.Traversal.Traversal' because
-- there are potential loss of state issues otherwise.
(<<~) :: MonadState s m => LensLike (Context a b) s s a b -> m b -> m b
l <<~ mb = do
  b <- mb
  modify $ \s -> case l (Context id) s of Context f _ -> f b
  return b
{-# INLINE (<<~) #-}

-- | Useful for storing lenses in containers.
newtype ReifiedLens s t a b = ReifyLens { reflectLens :: Lens s t a b }

-- | @type 'SimpleReifiedLens' = 'Simple' 'ReifiedLens'@
type SimpleReifiedLens s a = ReifiedLens s s a a

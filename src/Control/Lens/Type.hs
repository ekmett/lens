{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
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
-- A @'Lens' a b c d@ is a purely functional reference.
--
-- While a 'Control.Lens.Traversal.Traversal' could be used for
-- 'Control.Lens.Getter.Getting' like a valid 'Control.Lens.Fold.Fold',
-- it wasn't a valid 'Control.Lens.Getter.Getter' as 'Applicative' wasn't a superclass of
-- 'Control.Lens.Getter.Gettable'.
--
-- 'Functor', however is the superclass of both.
--
-- @type 'Lens' a b c d = forall f. 'Functor' f => (c -> f d) -> a -> f b@
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
  , (:->)

  , lens
  , simple
  , (%%~)
  , (%%=)

  , resultAt

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
  ) where

import Control.Applicative              as Applicative
import Control.Lens.Internal
import Control.Monad.State.Class        as State

-- $setup
-- >>> import Control.Lens

-- types
infixr 0 :->

-- terms
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
-- With great power comes great responsibility and a 'Lens'is subject to the
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
-- @type 'Lens' a b c d = forall f. 'Functor' f => 'LensLike' f a b c d@
type Lens a b c d = forall f. Functor f => (c -> f d) -> a -> f b

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
type Simple f a b = f a a b b

-- | This is a commonly used infix alias for a @'Simple' 'Lens'@.
type a :-> b = forall f. Functor f => (b -> f b) -> a -> f a

-- | @type 'SimpleLens' = 'Simple' 'Lens'@
type SimpleLens a b = Lens a a b b

-- | @type 'SimpleLensLike' f = 'Simple' ('LensLike' f)@
type SimpleLensLike f a b = LensLike f a a b b

--------------------------
-- Constructing Lenses
--------------------------

-- | Build a 'Lens' from a getter and a setter.
--
-- @'lens' :: 'Functor' f => (a -> c) -> (a -> d -> b) -> (c -> f d) -> a -> f b@
lens :: (a -> c) -> (a -> d -> b) -> Lens a b c d
lens ac adb cfd a = adb a <$> cfd (ac a)
{-# INLINE lens #-}

-- | This is occasionally useful when your 'Lens' (or 'Control.Lens.Traversal.Traversal')
-- has a constraint on an unused argument to force that argument to agree with the
-- type of a used argument and avoid @ScopedTypeVariables@ or other ugliness.
simple :: SimpleLensLike f a b -> SimpleLensLike f a b
simple l = l

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
-- If a function accepts a @'LensLike' f a b c d@ for some 'Functor' @f@,
-- then they may be passed a 'Lens'.
--
-- Further, if @f@ is an 'Applicative', they may also be passed a
-- 'Control.Lens.Traversal.Traversal'.
type LensLike f a b c d = (c -> f d) -> a -> f b

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
-- ('%%~') :: 'Functor' f =>     'Control.Lens.Iso.Iso' a b c d       -> (c -> f d) -> a -> f b
-- ('%%~') :: 'Functor' f =>     'Lens' a b c d      -> (c -> f d) -> a -> f b
-- ('%%~') :: 'Applicative' f => 'Control.Lens.Traversal.Traversal' a b c d -> (c -> f d) -> a -> f b
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
-- ('%%~') ::             'Control.Lens.Iso.Iso' a b c d       -> (c -> (e, d)) -> a -> (e, b)
-- ('%%~') ::             'Lens' a b c d      -> (c -> (e, d)) -> a -> (e, b)
-- ('%%~') :: 'Monoid' m => 'Control.Lens.Traversal.Traversal' a b c d -> (c -> (m, d)) -> a -> (m, b)
-- @
(%%~) :: LensLike f a b c d -> (c -> f d) -> a -> f b
(%%~) = id
{-# INLINE (%%~) #-}

-- | Modify the target of a 'Lens' in the current state returning some extra
-- information of @c@ or modify all targets of a
-- 'Control.Lens.Traversal.Traversal' in the current state, extracting extra
-- information of type @c@ and return a monoidal summary of the changes.
--
-- @('%%=') ≡ ('state' '.')@
--
-- It may be useful to think of ('%%='), instead, as having either of the
-- following more restricted type signatures:
--
-- @
-- ('%%=') :: 'MonadState' a m             => 'Control.Lens.Iso.Iso' a a c d       -> (c -> (e, d) -> m e
-- ('%%=') :: 'MonadState' a m             => 'Lens' a a c d      -> (c -> (e, d) -> m e
-- ('%%=') :: ('MonadState' a m, 'Monoid' e) => 'Control.Lens.Traversal.Traversal' a a c d -> (c -> (e, d) -> m e
-- @
(%%=) :: MonadState a m => LensLike ((,) e) a a c d -> (c -> (e, d)) -> m e
#if MIN_VERSION_mtl(2,1,1)
l %%= f = State.state (l f)
#else
l %%= f = do
  (e, b) <- State.gets (l f)
  State.put b
  return e
#endif
{-# INLINE (%%=) #-}

-------------------------------------------------------------------------------
-- Common Lenses
-------------------------------------------------------------------------------

-- | This lens can be used to change the result of a function but only where
-- the arguments match the key given.
resultAt :: Eq e => e -> (e -> a) :-> a
resultAt e afa ea = go <$> afa a where
  a = ea e
  go a' e' | e == e'   = a'
           | otherwise = a
{-# INLINE resultAt #-}

-- | Merge two lenses, getters, setters, folds or traversals.
--
-- @'chosen' ≡ 'choosing' 'id' 'id'@
--
-- @
-- 'choosing' :: 'Control.Lens.Getter.Getter' a c           -> 'Control.Lens.Getter.Getter' b c           -> 'Control.Lens.Getter.Getter' ('Either' a b) c
-- 'choosing' :: 'Control.Lens.Fold.Fold' a c             -> 'Control.Lens.Fold.Fold' b c             -> 'Control.Lens.Fold.Fold' ('Either' a b) c
-- 'choosing' :: 'Simple' 'Lens' a c      -> 'Simple' 'Lens' b c      -> 'Simple' 'Lens' ('Either' a b) c
-- 'choosing' :: 'Simple' 'Control.Lens.Traversal.Traversal' a c -> 'Simple' 'Control.Lens.Traversal.Traversal' b c -> 'Simple' 'Control.Lens.Traversal.Traversal' ('Either' a b) c
-- 'choosing' :: 'Simple' 'Control.Lens.Setter.Setter' a c    -> 'Simple' 'Control.Lens.Setter.Setter' b c    -> 'Simple' 'Control.Lens.Setter.Setter' ('Either' a b) c
-- @
choosing :: Functor f
       => LensLike f a b c c
       -> LensLike f a' b' c c
       -> LensLike f (Either a a') (Either b b') c c
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
-- @'alongside' :: 'Lens' a b c d -> 'Lens' a' b' c' d' -> 'Lens' (a,a') (b,b') (c,c') (d,d')@
alongside :: LensLike (Context c d) a b c d
           -> LensLike (Context c' d')  a' b' c' d'
           -> Lens (a,a') (b,b') (c,c') (d,d')
alongside l r f (a, a') = case l (Context id) a of
  Context db c -> case r (Context id) a' of
    Context db' c' -> (\(d,d') -> (db d, db' d')) <$> f (c,c')
{-# INLINE alongside #-}

-------------------------------------------------------------------------------
-- Cloning Lenses
-------------------------------------------------------------------------------

-- |
-- Cloning a 'Lens' is one way to make sure you arent given
-- something weaker, such as a 'Control.Lens.Traversal.Traversal' and can be
-- used as a way to pass around lenses that have to be monomorphic in @f@.
--
-- Note: This only accepts a proper 'Lens'.
--
-- /\"Costate Comonad Coalgebra is equivalent of Java's member variable update technology for Haskell\"/ -- \@PLT_Borat on Twitter
cloneLens :: Functor f
  => LensLike (Context c d) a b c d
  -> (c -> f d) -> a -> f b
cloneLens f cfd a = case f (Context id) a of
  Context db c -> db <$> cfd c
{-# INLINE cloneLens #-}

-------------------------------------------------------------------------------
-- Overloading function application
-------------------------------------------------------------------------------

-- | @type 'LensLike' f a b c d = 'Overloaded' (->) f a b c d@
type Overloaded k f a b c d = k (c -> f d) (a -> f b)

-- | @type 'SimpleOverloaded' k f a b = 'Simple' ('Overloaded' k f) a b@
type SimpleOverloaded k f a b = Overloaded k f a a b b

-------------------------------------------------------------------------------
-- Setting and Remembering
-------------------------------------------------------------------------------

-- | Modify the target of a 'Lens' and return the result
--
-- When you do not need the result of the addition, ('Control.Lens.Setter.%~') is more flexible.
--
-- @
-- ('<%~') ::             'Lens' a b c d      -> (c -> d) -> a -> (d, b)
-- ('<%~') ::             'Control.Lens.Iso.Iso' a b c d       -> (c -> d) -> a -> (d, b)
-- ('<%~') :: 'Monoid' d => 'Control.Lens.Traversal.Traversal' a b c d -> (c -> d) -> a -> (d, b)
-- @
(<%~) :: LensLike ((,)d) a b c d -> (c -> d) -> a -> (d, b)
l <%~ f = l $ \c -> let d = f c in (d, d)
{-# INLINE (<%~) #-}

-- | Increment the target of a numerically valued 'Lens' and return the result
--
-- When you do not need the result of the addition, ('Control.Lens.Setter.+~') is more flexible.
--
-- @
-- ('<+~') :: 'Num' b => 'Simple' 'Lens' a b -> b -> a -> (b, a)
-- ('<+~') :: 'Num' b => 'Simple' 'Control.Lens.Iso.Iso' a b  -> b -> a -> (b, a)
-- @
(<+~) :: Num c => LensLike ((,)c) a b c c -> c -> a -> (c, b)
l <+~ c = l <%~ (+ c)
{-# INLINE (<+~) #-}

-- | Decrement the target of a numerically valued 'Lens' and return the result
--
-- When you do not need the result of the subtraction, ('Control.Lens.Setter.-~') is more flexible.
--
-- @
-- ('<-~') :: 'Num' b => 'Simple' 'Lens' a b -> b -> a -> (b, a)
-- ('<-~') :: 'Num' b => 'Simple' 'Control.Lens.Iso.Iso' a b  -> b -> a -> (b, a)
-- @
(<-~) :: Num c => LensLike ((,)c) a b c c -> c -> a -> (c, b)
l <-~ c = l <%~ subtract c
{-# INLINE (<-~) #-}

-- | Multiply the target of a numerically valued 'Lens' and return the result
--
-- When you do not need the result of the multiplication, ('Control.Lens.Setter.*~') is more
-- flexible.
--
-- @
-- ('<*~') :: 'Num' b => 'Simple' 'Lens' a b -> b -> a -> (b, a)
-- ('<*~') :: 'Num' b => 'Simple' 'Control.Lens.Iso.Iso' a b  -> b -> a -> (b, a)
-- @
(<*~) :: Num c => LensLike ((,)c) a b c c -> c -> a -> (c, b)
l <*~ c = l <%~ (* c)
{-# INLINE (<*~) #-}

-- | Divide the target of a fractionally valued 'Lens' and return the result.
--
-- When you do not need the result of the division, ('Control.Lens.Setter.//~') is more flexible.
--
-- @
-- ('<//~') :: 'Fractional' b => 'Simple' 'Lens' a b -> b -> a -> (b, a)
-- ('<//~') :: 'Fractional' b => 'Simple' 'Control.Lens.Iso.Iso' a b  -> b -> a -> (b, a)
-- @
(<//~) :: Fractional c => LensLike ((,)c) a b c c -> c -> a -> (c, b)
l <//~ c = l <%~ (/ c)
{-# INLINE (<//~) #-}

-- | Raise the target of a numerically valued 'Lens' to a non-negative
-- 'Integral' power and return the result
--
-- When you do not need the result of the division, ('Control.Lens.Setter.^~') is more flexible.
--
-- @
-- ('<^~') :: ('Num' b, 'Integral' c) => 'Simple' 'Lens' a b -> c -> a -> (b, a)
-- ('<^~') :: ('Num' b, 'Integral' c) => 'Simple' 'Control.Lens.Iso.Iso' a b  -> c -> a -> (b, a)
-- @
(<^~) :: (Num c, Integral d) => LensLike ((,)c) a b c c -> d -> a -> (c, b)
l <^~ d = l <%~ (^ d)
{-# INLINE (<^~) #-}

-- | Raise the target of a fractionally valued 'Lens' to an 'Integral' power
-- and return the result.
--
-- When you do not need the result of the division, ('Control.Lens.Setter.^^~') is more flexible.
--
-- @
-- ('<^^~') :: ('Fractional' b, 'Integral' c) => 'Simple' 'Lens' a b -> c -> a -> (b, a)
-- ('<^^~') :: ('Fractional' b, 'Integral' c) => 'Simple' 'Control.Lens.Iso.Iso' a b  -> c -> a -> (b, a)
-- @
(<^^~) :: (Fractional c, Integral d) => LensLike ((,)c) a b c c -> d -> a -> (c, b)
l <^^~ d = l <%~ (^^ d)
{-# INLINE (<^^~) #-}

-- | Raise the target of a floating-point valued 'Lens' to an arbitrary power
-- and return the result.
--
-- When you do not need the result of the division, ('Control.Lens.Setter.**~') is more flexible.
--
-- @
-- ('<**~') :: 'Floating' b => 'Simple' 'Lens' a b -> b -> a -> (b, a)
-- ('<**~') :: 'Floating' b => 'Simple' 'Control.Lens.Iso.Iso' a b  -> b -> a -> (b, a)
-- @
(<**~) :: Floating c => LensLike ((,)c) a b c c -> c -> a -> (c, b)
l <**~ c = l <%~ (** c)
{-# INLINE (<**~) #-}

-- | Logically '||' a Boolean valued 'Lens' and return the result
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.||~') is more flexible.
--
-- @
-- ('<||~') :: 'Simple' 'Lens' a 'Bool' -> 'Bool' -> a -> ('Bool', a)
-- ('<||~') :: 'Simple' 'Control.Lens.Iso.Iso' a 'Bool'  -> 'Bool' -> a -> ('Bool', a)
-- @
(<||~) :: LensLike ((,)Bool) a b Bool Bool -> Bool -> a -> (Bool, b)
l <||~ c = l <%~ (|| c)
{-# INLINE (<||~) #-}

-- | Logically '&&' a Boolean valued 'Lens' and return the result
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.&&~') is more flexible.
--
-- @
-- ('<&&~') :: 'Simple' 'Lens' a 'Bool' -> 'Bool' -> a -> ('Bool', a)
-- ('<&&~') :: 'Simple' 'Control.Lens.Iso.Iso' a 'Bool'  -> 'Bool' -> a -> ('Bool', a)
-- @
(<&&~) :: LensLike ((,)Bool) a b Bool Bool -> Bool -> a -> (Bool, b)
l <&&~ c = l <%~ (&& c)
{-# INLINE (<&&~) #-}

-- | Modify the target of a 'Lens', but return the old value.
--
-- When you do not need the result of the addition, ('Control.Lens.Setter.%~') is more flexible.
--
-- @
-- ('<<%~') ::             'Lens' a b c d      -> (c -> d) -> a -> (d, b)
-- ('<<%~') ::             'Control.Lens.Iso.Iso' a b c d       -> (c -> d) -> a -> (d, b)
-- ('<<%~') :: 'Monoid' d => 'Control.Lens.Traversal.Traversal' a b c d -> (c -> d) -> a -> (d, b)
-- @
(<<%~) :: LensLike ((,)c) a b c d -> (c -> d) -> a -> (c, b)
l <<%~ f = l $ \c -> (c, f c)
{-# INLINE (<<%~) #-}

-- | Modify the target of a 'Lens', but return the old value.
--
-- When you do not need the old value, ('Control.Lens.Setter.%~') is more flexible.
--
-- @
-- ('<<%~') ::             'Lens' a b c d      -> d -> a -> (c, b)
-- ('<<%~') ::             'Control.Lens.Iso.Iso' a b c d       -> d -> a -> (c, b)
-- ('<<%~') :: 'Monoid' d => 'Control.Lens.Traversal.Traversal' a b c d -> d -> a -> (c, b)
-- @
(<<.~) :: LensLike ((,)c) a b c d -> d -> a -> (c, b)
l <<.~ d = l $ \c -> (c, d)
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
-- ('<%=') :: 'MonadState' a m             => 'Simple' 'Lens' a b     -> (b -> b) -> m b
-- ('<%=') :: 'MonadState' a m             => 'Simple' 'Control.Lens.Iso.Iso' a b      -> (b -> b) -> m b
-- ('<%=') :: ('MonadState' a m, 'Monoid' b) => 'Simple' 'Traveral' a b -> (b -> b) -> m b
-- @
(<%=) :: MonadState a m => LensLike ((,)d) a a c d -> (c -> d) -> m d
l <%= f = l %%= \c -> let d = f c in (d,d)
{-# INLINE (<%=) #-}


-- | Add to the target of a numerically valued 'Lens' into your monad's state
-- and return the result.
--
-- When you do not need the result of the addition, ('Control.Lens.Setter.+=') is more
-- flexible.
--
-- @
-- ('<+=') :: ('MonadState' a m, 'Num' b) => 'Simple' 'Lens' a b -> b -> m b
-- ('<+=') :: ('MonadState' a m, 'Num' b) => 'Simple' 'Control.Lens.Iso.Iso' a b  -> b -> m b
-- @
(<+=) :: (MonadState a m, Num b) => SimpleLensLike ((,)b) a b -> b -> m b
l <+= b = l <%= (+ b)
{-# INLINE (<+=) #-}

-- | Subtract from the target of a numerically valued 'Lens' into your monad's
-- state and return the result.
--
-- When you do not need the result of the subtraction, ('Control.Lens.Setter.-=') is more
-- flexible.
--
-- @
-- ('<-=') :: ('MonadState' a m, 'Num' b) => 'Simple' 'Lens' a b -> b -> m b
-- ('<-=') :: ('MonadState' a m, 'Num' b) => 'Simple' 'Control.Lens.Iso.Iso' a b  -> b -> m b
-- @
(<-=) :: (MonadState a m, Num b) => SimpleLensLike ((,)b) a b -> b -> m b
l <-= b = l <%= subtract b
{-# INLINE (<-=) #-}

-- | Multiply the target of a numerically valued 'Lens' into your monad's
-- state and return the result.
--
-- When you do not need the result of the multiplication, ('Control.Lens.Setter.*=') is more
-- flexible.
--
-- @
-- ('<*=') :: ('MonadState' a m, 'Num' b) => 'Simple' 'Lens' a b -> b -> m b
-- ('<*=') :: ('MonadState' a m, 'Num' b) => 'Simple' 'Control.Lens.Iso.Iso' a b  -> b -> m b
-- @
(<*=) :: (MonadState a m, Num b) => SimpleLensLike ((,)b) a b -> b -> m b
l <*= b = l <%= (* b)
{-# INLINE (<*=) #-}

-- | Divide the target of a fractionally valued 'Lens' into your monad's state
-- and return the result.
--
-- When you do not need the result of the division, ('Control.Lens.Setter.//=') is more flexible.
--
-- @
-- ('<//=') :: ('MonadState' a m, 'Fractional' b) => 'Simple' 'Lens' a b -> b -> m b
-- ('<//=') :: ('MonadState' a m, 'Fractional' b) => 'Simple' 'Control.Lens.Iso.Iso' a b  -> b -> m b
-- @
(<//=) :: (MonadState a m, Fractional b) => SimpleLensLike ((,)b) a b -> b -> m b
l <//= b = l <%= (/ b)
{-# INLINE (<//=) #-}

-- | Raise the target of a numerically valued 'Lens' into your monad's state
-- to a non-negative 'Integral' power and return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.**=') is more flexible.
--
-- @
-- ('<^=') :: ('MonadState' a m, 'Num' b, 'Integral' c) => 'Simple' 'Lens' a b -> c -> m b
-- ('<^=') :: ('MonadState' a m, 'Num' b, 'Integral' c) => 'Simple' 'Control.Lens.Iso.Iso' a b  -> c -> m b
-- @
(<^=) :: (MonadState a m, Num b, Integral c) => SimpleLensLike ((,)b) a b -> c -> m b
l <^= c = l <%= (^ c)
{-# INLINE (<^=) #-}

-- | Raise the target of a fractionally valued 'Lens' into your monad's state
-- to an 'Integral' power and return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.^^=') is more flexible.
--
-- @
-- ('<^^=') :: ('MonadState' a m, 'Fractional' b, 'Integral' c) => 'Simple' 'Lens' a b -> c -> m b
-- ('<^^=') :: ('MonadState' a m, 'Fractional' b, 'Integral' c) => 'Simple' 'Control.Lens.Iso.Iso' a b  -> c -> m b
-- @
(<^^=) :: (MonadState a m, Fractional b, Integral c) => SimpleLensLike ((,)b) a b -> c -> m b
l <^^= c = l <%= (^^ c)
{-# INLINE (<^^=) #-}

-- | Raise the target of a floating-point valued 'Lens' into your monad's
-- state to an arbitrary power and return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.**=') is more flexible.
--
-- @
-- ('<**=') :: ('MonadState' a m, 'Floating' b) => 'Simple' 'Lens' a b -> b -> m b
-- ('<**=') :: ('MonadState' a m, 'Floating' b) => 'Simple' 'Control.Lens.Iso.Iso' a b  -> b -> m b
-- @
(<**=) :: (MonadState a m, Floating b) => SimpleLensLike ((,)b) a b -> b -> m b
l <**= b = l <%= (** b)
{-# INLINE (<**=) #-}

-- | Logically '||' a Boolean valued 'Lens' into your monad's state and return
-- the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.||=') is more flexible.
--
-- @
-- ('<||=') :: 'MonadState' a m => 'Simple' 'Lens' a 'Bool' -> 'Bool' -> m 'Bool'
-- ('<||=') :: 'MonadState' a m => 'Simple' 'Control.Lens.Iso.Iso' a 'Bool'  -> 'Bool' -> m 'Bool'
-- @
(<||=) :: MonadState a m => SimpleLensLike ((,)Bool) a Bool -> Bool -> m Bool
l <||= b = l <%= (|| b)
{-# INLINE (<||=) #-}

-- | Logically '&&' a Boolean valued 'Lens' into your monad's state and return
-- the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.&&=') is more flexible.
--
-- @
-- ('<&&=') :: 'MonadState' a m => 'Simple' 'Lens' a 'Bool' -> 'Bool' -> m 'Bool'
-- ('<&&=') :: 'MonadState' a m => 'Simple' 'Control.Lens.Iso.Iso' a 'Bool'  -> 'Bool' -> m 'Bool'
-- @
(<&&=) :: MonadState a m => SimpleLensLike ((,)Bool) a Bool -> Bool -> m Bool
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
-- ('<<%=') :: 'MonadState' a m             => 'Simple' 'Lens' a b     -> (b -> b) -> m b
-- ('<<%=') :: 'MonadState' a m             => 'Simple' 'Control.Lens.Iso.Iso' a b      -> (b -> b) -> m b
-- ('<<%=') :: ('MonadState' a m, 'Monoid' b) => 'Simple' 'Traveral' a b -> (b -> b) -> m b
-- @
(<<%=) :: MonadState a m => LensLike ((,)c) a a c d -> (c -> d) -> m c
l <<%= f = l %%= \c -> (c, f c)
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
-- ('<<%=') :: 'MonadState' a m             => 'Simple' 'Lens' a b     -> (b -> b) -> m b
-- ('<<%=') :: 'MonadState' a m             => 'Simple' 'Control.Lens.Iso.Iso' a b      -> (b -> b) -> m b
-- ('<<%=') :: ('MonadState' a m, 'Monoid' b) => 'Simple' 'Traveral' a b -> (b -> b) -> m b
-- @
(<<.=) :: MonadState a m => LensLike ((,)c) a a c d -> d -> m c
l <<.= d = l %%= \c -> (c,d)
{-# INLINE (<<.=) #-}

-- | Run a monadic action, and set the target of 'Lens' to its result.
--
-- @
-- ('<<~') :: 'MonadState' a m => 'Control.Lens.Iso.Iso' a a c d   -> m d -> m d
-- ('<<~') :: 'MonadState' a m => 'Control.Lens.Type.Lens' a a c d  -> m d -> m d
-- @
--
-- NB: This is limited to taking an actual 'Lens' than admitting a 'Control.Lens.Traversal.Traversal' because
-- there are potential loss of state issues otherwise.
(<<~) :: MonadState a m => LensLike (Context c d) a a c d -> m d -> m d
l <<~ md = do
  d <- md
  modify $ \a -> case l (Context id) a of Context f _ -> f d
  return d
{-# INLINE (<<~) #-}

-- | Useful for storing lenses in containers.
newtype ReifiedLens a b c d = ReifyLens { reflectLens :: Lens a b c d }

-- | @type 'SimpleReifiedLens' = 'Simple' 'ReifiedLens'@
type SimpleReifiedLens a b = ReifiedLens a a b b

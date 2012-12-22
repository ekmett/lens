{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x,y,z) 1
#endif
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Lens
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
--
-- In the examples below, 'getter' and 'setter' are supplied as example getters
-- and setters, and are not actual functions supplied by this package.
-------------------------------------------------------------------------------
module Control.Lens.Lens
  (
  -- * Lenses
    Lens, Lens'
  -- ** Concrete Lenses
  , ALens, ALens'

  -- * Combinators
  , lens
  , (%%~), (%%=)

  -- * Indexed
  , IndexedLens, IndexedLens'
  , (%%@~), (%%@=)
  , (<%@~), (<%@=)

  -- ** Lateral Composition
  , choosing
  , chosen
  , alongside
  , inside

  -- ** Setting Functionally with Passthrough
  , (<%~), (<+~), (<-~), (<*~), (<//~)
  , (<^~), (<^^~), (<**~)
  , (<||~), (<&&~), (<<>~)
  , (<<%~), (<<.~)

  -- ** Setting State with Passthrough
  , (<%=), (<+=), (<-=), (<*=), (<//=)
  , (<^=), (<^^=), (<**=)
  , (<||=), (<&&=), (<<>=)
  , (<<%=), (<<.=)
  , (<<~)

  -- ** Cloning Lenses
  , cloneLens

  -- * Context
  , Context(..)
  , locus
  ) where

import Control.Applicative
import Control.Comonad.Store as Store
import Control.Lens.Combinators
import Control.Lens.Internal
import Control.Lens.Type
import Control.Monad.State as State
import Data.Monoid

{-# ANN module "HLint: ignore Use ***" #-}

-- $setup
-- >>> import Control.Lens
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g,h)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g
-- >>> let h :: Expr -> Expr -> Expr; h = Debug.SimpleReflect.Vars.h
-- >>> let getter :: Expr -> Expr; getter = fun "getter"
-- >>> let setter :: Expr -> Expr -> Expr; setter = fun "setter"

infixr 4 %%@~, <%@~, %%~, <+~, <*~, <-~, <//~, <^~, <^^~, <**~, <&&~, <||~, <<>~, <%~, <<%~, <<.~
infix  4 %%@=, <%@=, %%=, <+=, <*=, <-=, <//=, <^=, <^^=, <**=, <&&=, <||=, <<>=, <%=, <<%=, <<.=
infixr 2 <<~

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

-- | When you see this as an argument to a function, it expects a 'Lens'.
type ALens s t a b = LensLike (Context a b) s t a b

-- | @type 'ALens'' = 'Simple' 'ALens'@
type ALens' s a = ALens s s a a

--------------------------
-- Constructing Lenses
--------------------------

-- | Build a 'Lens' from a getter and a setter.
--
-- @'lens' :: 'Functor' f => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t@
--
-- >>> s ^. lens getter setter
-- getter s
--
-- >>> s & lens getter setter .~ b
-- setter s b
--
-- >>> s & lens getter setter %~ f
-- setter s (f (getter s))
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

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
-- >>> runState (_1 %%= \x -> (f x, g x)) (a,b)
-- (f a,(g a,b))
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

-- | Lift a 'Lens' so it can run under a function.
--
inside :: ALens s t a b -> Lens (e -> s) (e -> t) (e -> a) (e -> b)
inside l f es = o <$> f i where
  i e = case l (Context id) (es e) of Context _ a -> a
  o ea e = case l (Context id) (es e) of Context k _ -> k (ea e)

-- | Merge two lenses, getters, setters, folds or traversals.
--
-- @'chosen' ≡ 'choosing' 'id' 'id'@
--
-- @
-- 'choosing' :: 'Control.Lens.Getter.Getter' s a           -> 'Control.Lens.Getter.Getter' s' a           -> 'Control.Lens.Getter.Getter' ('Either' s s') a
-- 'choosing' :: 'Control.Lens.Fold.Fold' s a             -> 'Control.Lens.Fold.Fold' s' a             -> 'Control.Lens.Fold.Fold' ('Either' s s') a
-- 'choosing' :: 'Lens'' s a      -> 'Lens'' s' a      -> 'Lens'' ('Either' s s') a
-- 'choosing' :: 'Control.Lens.Traversal.Traversal'' s a -> 'Control.Lens.Traversal.Traversal'' s' a -> 'Control.Lens.Traversal.Traversal'' ('Either' s s') a
-- 'choosing' :: 'Control.Lens.Setter.Setter'' s a    -> 'Control.Lens.Setter.Setter'' s' a    -> 'Control.Lens.Setter.Setter'' ('Either' s s') a
-- @
choosing :: Functor f
       => LensLike f s t a b
       -> LensLike f s' t' a b
       -> LensLike f (Either s s') (Either t t') a b
choosing l _ f (Left a)   = Left <$> l f a
choosing _ r f (Right a') = Right <$> r f a'
{-# INLINE choosing #-}

-- | This is a 'Lens' that updates either side of an 'Either', where both sides have the same type.
--
-- @'chosen' ≡ 'choosing' 'id' 'id'@
--
-- >>> Left a^.chosen
-- a
--
-- >>> Right a^.chosen
-- a
--
-- >>> Right "hello"^.chosen
-- "hello"
--
-- >>> Right a & chosen *~ b
-- Right (a * b)
chosen :: Lens (Either a a) (Either b b) a b
chosen f (Left a) = Left <$> f a
chosen f (Right a) = Right <$> f a
{-# INLINE chosen #-}

-- | 'alongside' makes a 'Lens' from two other lenses.
--
-- >>> (Left a, Right b)^.alongside chosen chosen
-- (a,b)
--
-- >>> (Left a, Right b) & alongside chosen chosen .~ (c,d)
-- (Left c,Right d)
--
-- @'alongside' :: 'Lens' s t a b -> 'Lens' s' t' a' b' -> 'Lens' (s,s') (t,t') (a,a') (b,b')@
alongside :: ALens s t a b -> ALens s' t' a' b' -> Lens (s,s') (t,t') (a,a') (b,b')
alongside l r f (s, s') = case l (Context id) s of
  Context bt a -> case r (Context id) s' of
    Context bt' a' -> f (a,a') <&> \(b,b') -> (bt b, bt' b')
{-# INLINE alongside #-}

-- | This 'Lens' lets you 'view' the current 'pos' of any 'Store'
-- 'Comonad' and 'seek' to a new position. This reduces the API
-- for working with a 'ComonadStore' instances to a single 'Lens'.
--
-- @
-- 'pos' w ≡ w '^.' 'locus'
-- 'seek' s w ≡ w '&' 'locus' '.~' s
-- 'seeks' f w ≡ w '&' 'locus' '%~' f
-- @
--
-- @
-- 'locus' :: 'Lens'' ('Context' s s a) s
-- @
locus :: ComonadStore s w => Lens' (w a) s
locus f w = (`seek` w) <$> f (pos w)

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
-- >>> let example l x = set (cloneLens l) (x^.cloneLens l + 1) x in example _2 ("hello",1,"you")
-- ("hello",2,"you")
cloneLens :: ALens s t a b -> Lens s t a b
cloneLens f afb s = case f (Context id) s of
  Context bt a -> bt <$> afb a
{-# INLINE cloneLens #-}

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
-- ('<+~') :: 'Num' a => 'Lens'' s a -> a -> s -> (a, s)
-- ('<+~') :: 'Num' a => 'Control.Lens.Iso.Iso'' s a  -> a -> s -> (a, s)
-- @
(<+~) :: Num a => LensLike ((,)a) s t a a -> a -> s -> (a, t)
l <+~ a = l <%~ (+ a)
{-# INLINE (<+~) #-}

-- | Decrement the target of a numerically valued 'Lens' and return the result
--
-- When you do not need the result of the subtraction, ('Control.Lens.Setter.-~') is more flexible.
--
-- @
-- ('<-~') :: 'Num' a => 'Lens'' s a -> a -> s -> (a, s)
-- ('<-~') :: 'Num' a => 'Control.Lens.Iso.Iso'' s a  -> a -> s -> (a, s)
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
-- ('<*~') :: 'Num' b => 'Lens'' s a -> a -> a -> (s, a)
-- ('<*~') :: 'Num' b => 'Control.Lens.Iso.Iso''  s a -> a -> a -> (s, a)
-- @
(<*~) :: Num a => LensLike ((,)a) s t a a -> a -> s -> (a, t)
l <*~ a = l <%~ (* a)
{-# INLINE (<*~) #-}

-- | Divide the target of a fractionally valued 'Lens' and return the result.
--
-- When you do not need the result of the division, ('Control.Lens.Setter.//~') is more flexible.
--
-- @
-- ('<//~') :: 'Fractional' b => 'Lens'' s a -> a -> a -> (s, a)
-- ('<//~') :: 'Fractional' b => 'Control.Lens.Iso.Iso''  s a -> a -> a -> (s, a)
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
-- ('<^~') :: ('Num' b, 'Integral' e) => 'Lens'' s a -> e -> a -> (a, s)
-- ('<^~') :: ('Num' b, 'Integral' e) => 'Control.Lens.Iso.Iso'' s a -> e -> a -> (a, s)
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
-- ('<^^~') :: ('Fractional' b, 'Integral' e) => 'Lens'' s a -> e -> a -> (a, s)
-- ('<^^~') :: ('Fractional' b, 'Integral' e) => 'Control.Lens.Iso.Iso'' s a -> e -> a -> (a, s)
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
-- ('<**~') :: 'Floating' a => 'Lens'' s a -> a -> s -> (a, s)
-- ('<**~') :: 'Floating' a => 'Control.Lens.Iso.Iso'' s a  -> a -> s -> (a, s)
-- @
(<**~) :: Floating a => LensLike ((,)a) s t a a -> a -> s -> (a, t)
l <**~ a = l <%~ (** a)
{-# INLINE (<**~) #-}

-- | Logically '||' a Boolean valued 'Lens' and return the result
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.||~') is more flexible.
--
-- @
-- ('<||~') :: 'Lens'' s 'Bool' -> 'Bool' -> s -> ('Bool', s)
-- ('<||~') :: 'Control.Lens.Iso.Iso'' s 'Bool'  -> 'Bool' -> s -> ('Bool', s)
-- @
(<||~) :: LensLike ((,)Bool) s t Bool Bool -> Bool -> s -> (Bool, t)
l <||~ b = l <%~ (|| b)
{-# INLINE (<||~) #-}

-- | Logically '&&' a Boolean valued 'Lens' and return the result
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.&&~') is more flexible.
--
-- @
-- ('<&&~') :: 'Lens'' s 'Bool' -> 'Bool' -> s -> ('Bool', s)
-- ('<&&~') :: 'Control.Lens.Iso.Iso'' s 'Bool'  -> 'Bool' -> s -> ('Bool', s)
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
-- ('<%=') :: 'MonadState' s m             => 'Lens'' s a     -> (a -> a) -> m a
-- ('<%=') :: 'MonadState' s m             => 'Control.Lens.Iso.Iso'' s a      -> (a -> a) -> m a
-- ('<%=') :: ('MonadState' s m, 'Monoid' a) => 'Control.Lens.Traversal.Traversal'' s a -> (a -> a) -> m a
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
-- ('<+=') :: ('MonadState' s m, 'Num' a) => 'Lens'' s a -> a -> m a
-- ('<+=') :: ('MonadState' s m, 'Num' a) => 'Control.Lens.Iso.Iso'' s a -> a -> m a
-- @
(<+=) :: (MonadState s m, Num a) => LensLike' ((,)a) s a -> a -> m a
l <+= a = l <%= (+ a)
{-# INLINE (<+=) #-}

-- | Subtract from the target of a numerically valued 'Lens' into your monad's
-- state and return the result.
--
-- When you do not need the result of the subtraction, ('Control.Lens.Setter.-=') is more
-- flexible.
--
-- @
-- ('<-=') :: ('MonadState' s m, 'Num' a) => 'Lens'' s a -> a -> m a
-- ('<-=') :: ('MonadState' s m, 'Num' a) => 'Control.Lens.Iso.Iso'' s a -> a -> m a
-- @
(<-=) :: (MonadState s m, Num a) => LensLike' ((,)a) s a -> a -> m a
l <-= a = l <%= subtract a
{-# INLINE (<-=) #-}

-- | Multiply the target of a numerically valued 'Lens' into your monad's
-- state and return the result.
--
-- When you do not need the result of the multiplication, ('Control.Lens.Setter.*=') is more
-- flexible.
--
-- @
-- ('<*=') :: ('MonadState' s m, 'Num' a) => 'Lens'' s a -> a -> m a
-- ('<*=') :: ('MonadState' s m, 'Num' a) => 'Control.Lens.Iso.Iso'' s a -> a -> m a
-- @
(<*=) :: (MonadState s m, Num a) => LensLike' ((,)a) s a -> a -> m a
l <*= a = l <%= (* a)
{-# INLINE (<*=) #-}

-- | Divide the target of a fractionally valued 'Lens' into your monad's state
-- and return the result.
--
-- When you do not need the result of the division, ('Control.Lens.Setter.//=') is more flexible.
--
-- @
-- ('<//=') :: ('MonadState' s m, 'Fractional' a) => 'Lens'' s a -> a -> m a
-- ('<//=') :: ('MonadState' s m, 'Fractional' a) => 'Control.Lens.Iso.Iso'' s a -> a -> m a
-- @
(<//=) :: (MonadState s m, Fractional a) => LensLike' ((,)a) s a -> a -> m a
l <//= a = l <%= (/ a)
{-# INLINE (<//=) #-}

-- | Raise the target of a numerically valued 'Lens' into your monad's state
-- to a non-negative 'Integral' power and return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.**=') is more flexible.
--
-- @
-- ('<^=') :: ('MonadState' s m, 'Num' a, 'Integral' e) => 'Lens'' s a -> e -> m a
-- ('<^=') :: ('MonadState' s m, 'Num' a, 'Integral' e) => 'Control.Lens.Iso.Iso'' s a -> e -> m a
-- @
(<^=) :: (MonadState s m, Num a, Integral e) => LensLike' ((,)a) s a -> e -> m a
l <^= e = l <%= (^ e)
{-# INLINE (<^=) #-}

-- | Raise the target of a fractionally valued 'Lens' into your monad's state
-- to an 'Integral' power and return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.^^=') is more flexible.
--
-- @
-- ('<^^=') :: ('MonadState' s m, 'Fractional' b, 'Integral' e) => 'Lens'' s a -> e -> m a
-- ('<^^=') :: ('MonadState' s m, 'Fractional' b, 'Integral' e) => 'Control.Lens.Iso.Iso'' s a  -> e -> m a
-- @
(<^^=) :: (MonadState s m, Fractional a, Integral e) => LensLike' ((,)a) s a -> e -> m a
l <^^= e = l <%= (^^ e)
{-# INLINE (<^^=) #-}

-- | Raise the target of a floating-point valued 'Lens' into your monad's
-- state to an arbitrary power and return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.**=') is more flexible.
--
-- @
-- ('<**=') :: ('MonadState' s m, 'Floating' a) => 'Lens'' s a -> a -> m a
-- ('<**=') :: ('MonadState' s m, 'Floating' a) => 'Control.Lens.Iso.Iso'' s a -> a -> m a
-- @
(<**=) :: (MonadState s m, Floating a) => LensLike' ((,)a) s a -> a -> m a
l <**= a = l <%= (** a)
{-# INLINE (<**=) #-}

-- | Logically '||' a Boolean valued 'Lens' into your monad's state and return
-- the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.||=') is more flexible.
--
-- @
-- ('<||=') :: 'MonadState' s m => 'Lens'' s 'Bool' -> 'Bool' -> m 'Bool'
-- ('<||=') :: 'MonadState' s m => 'Control.Lens.Iso.Iso'' s 'Bool'  -> 'Bool' -> m 'Bool'
-- @
(<||=) :: MonadState s m => LensLike' ((,)Bool) s Bool -> Bool -> m Bool
l <||= b = l <%= (|| b)
{-# INLINE (<||=) #-}

-- | Logically '&&' a Boolean valued 'Lens' into your monad's state and return
-- the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.&&=') is more flexible.
--
-- @
-- ('<&&=') :: 'MonadState' s m => 'Lens'' s 'Bool' -> 'Bool' -> m 'Bool'
-- ('<&&=') :: 'MonadState' s m => 'Control.Lens.Iso.Iso'' s 'Bool'  -> 'Bool' -> m 'Bool'
-- @
(<&&=) :: MonadState s m => LensLike' ((,)Bool) s Bool -> Bool -> m Bool
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
-- ('<<%=') :: 'MonadState' s m             => 'Lens'' s a     -> (a -> a) -> m a
-- ('<<%=') :: 'MonadState' s m             => 'Control.Lens.Iso.Iso'' s a      -> (a -> a) -> m a
-- ('<<%=') :: ('MonadState' s m, 'Monoid' b) => 'Control.Lens.Traversal.Traversal'' s a -> (a -> a) -> m a
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
-- ('<<%=') :: 'MonadState' s m             => 'Lens'' s a     -> (a -> a) -> m a
-- ('<<%=') :: 'MonadState' s m             => 'Control.Lens.Iso.Iso'' s a      -> (a -> a) -> m a
-- ('<<%=') :: ('MonadState' s m, 'Monoid' t) => 'Control.Lens.Traversal.Traversal'' s a -> (a -> a) -> m a
-- @
(<<.=) :: MonadState s m => LensLike ((,)a) s s a b -> b -> m a
l <<.= b = l %%= \a -> (a,b)
{-# INLINE (<<.=) #-}

-- | Run a monadic action, and set the target of 'Lens' to its result.
--
-- @
-- ('<<~') :: 'MonadState' s m => 'Control.Lens.Iso.Iso' s s a b   -> m b -> m b
-- ('<<~') :: 'MonadState' s m => 'Lens' s s a b  -> m b -> m b
-- @
--
-- NB: This is limited to taking an actual 'Lens' than admitting a 'Control.Lens.Traversal.Traversal' because
-- there are potential loss of state issues otherwise.
(<<~) :: MonadState s m => ALens s s a b -> m b -> m b
l <<~ mb = do
  b <- mb
  modify $ \s -> case l (Context id) s of Context f _ -> f b
  return b
{-# INLINE (<<~) #-}

-- | 'mappend' a monoidal value onto the end of the target of a 'Lens' and
-- return the result
--
-- When you do not need the result of the operation, ('<>~') is more flexible.
(<<>~) :: Monoid m => LensLike ((,)m) s t m m -> m -> s -> (m, t)
l <<>~ m = l <%~ (`mappend` m)
{-# INLINE (<<>~) #-}

-- | 'mappend' a monoidal value onto the end of the target of a 'Lens' into
-- your monad's state and return the result.
--
-- When you do not need the result of the operation, ('<>=') is more flexible.
(<<>=) :: (MonadState s m, Monoid r) => LensLike' ((,)r) s r -> r -> m r
l <<>= r = l <%= (`mappend` r)
{-# INLINE (<<>=) #-}

------------------------------------------------------------------------------
-- Indexed
------------------------------------------------------------------------------

-- | Adjust the target of an 'IndexedLens' returning the intermediate result, or
-- adjust all of the targets of an 'Control.Lens.Traversal.IndexedTraversal' and return a monoidal summary
-- along with the answer.
--
-- @l '<%~' f ≡ l '<%@~' 'const' f@
--
-- When you do not need access to the index then ('<%~') is more liberal in what it can accept.
--
-- If you do not need the intermediate result, you can use ('%@~') or even ('%~').
--
-- @
-- ('<%@~') ::             'IndexedLens' i s t a b -> (i -> a -> b) -> s -> (b, t)
-- ('<%@~') :: 'Monoid' b => 'Control.Lens.Traversal.IndexedTraversal' i s t a b -> (i -> a -> b) -> s -> (b, t)
-- @
(<%@~) :: IndexedLensLike (Indexed i) ((,) b) s t a b -> (i -> a -> b) -> s -> (b, t)
l <%@~ f = l . Indexed $ \i a -> let b = f i a in (b, b)
{-# INLINE (<%@~) #-}

-- | Adjust the target of an 'IndexedLens' returning a supplementary result, or
-- adjust all of the targets of an 'Control.Lens.Traversal.IndexedTraversal' and return a monoidal summary
-- of the supplementary results and the answer.
--
-- @('%%@~') ≡ 'withIndex'@
--
-- @
-- ('%%@~') :: 'Functor' f => 'IndexedLens' i s t a b      -> (i -> a -> f b) -> s -> f t
-- ('%%@~') :: 'Functor' f => 'Control.Lens.Traversal.IndexedTraversal' i s t a b -> (i -> a -> f b) -> s -> f t
-- @
--
-- In particular, it is often useful to think of this function as having one of these even more
-- restrictive type signatures
--
-- @
-- ('%%@~') ::             'IndexedLens' i s t a b      -> (i -> a -> (r, b)) -> s -> (r, t)
-- ('%%@~') :: 'Monoid' r => 'Control.Lens.Traversal.IndexedTraversal' i s t a b -> (i -> a -> (r, b)) -> s -> (r, t)
-- @
(%%@~) :: IndexedLensLike (Indexed i) f s t a b -> (i -> a -> f b) -> s -> f t
(%%@~) l = l . Indexed
{-# INLINE (%%@~) #-}

-- | Adjust the target of an 'IndexedLens' returning a supplementary result, or
-- adjust all of the targets of an 'Control.Lens.Traversal.IndexedTraversal' within the current state, and
-- return a monoidal summary of the supplementary results.
--
-- @l '%%@=' f ≡ 'state' (l '%%@~' f)@
--
-- @
-- ('%%@=') :: 'MonadState' s m                'IndexedLens' i s s a b      -> (i -> a -> (r, b)) -> s -> m r
-- ('%%@=') :: ('MonadState' s m, 'Monoid' r) => 'Control.Lens.Traversal.IndexedTraversal' i s s a b -> (i -> a -> (r, b)) -> s -> m r
-- @
(%%@=) :: MonadState s m => IndexedLensLike (Indexed i) ((,) r) s s a b -> (i -> a -> (r, b)) -> m r
#if MIN_VERSION_mtl(2,1,0)
l %%@= f = State.state (l %%@~ f)
#else
l %%@= f = do
  (r, s) <- State.gets (l %%@~ f)
  State.put s
  return r
#endif
{-# INLINE (%%@=) #-}

-- | Adjust the target of an 'IndexedLens' returning the intermediate result, or
-- adjust all of the targets of an 'Control.Lens.Traversal.IndexedTraversal' within the current state, and
-- return a monoidal summary of the intermediate results.
--
-- @
-- ('<%@=') :: 'MonadState' s m                'IndexedLens' i s s a b      -> (i -> a -> b) -> m b
-- ('<%@=') :: ('MonadState' s m, 'Monoid' b) => 'Control.Lens.Traversal.IndexedTraversal' i s s a b -> (i -> a -> b) -> m b
-- @
(<%@=) :: MonadState s m => IndexedLensLike (Indexed i) ((,) b) s s a b -> (i -> a -> b) -> m b
l <%@= f = l %%@= \ i a -> let b = f i a in (b, b)
{-# INLINE (<%@=) #-}

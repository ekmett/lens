{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}

#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x,y,z) 1
#endif

-----------------------------------------------------------------------------
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
-- While a 'Control.Lens.Traversal.Traversal' could be used for 'Control.Lens.Getter.Getting' like a valid 'Control.Lens.Fold.Fold',
-- it wasn't a valid 'Getter' as Applicative isn't a superclass of 
-- 'Gettable'.
--
-- 'Functor', however is the superclass of both.
--
-- @type 'Lens' a b c d = forall f. 'Functor' f => (c -> f d) -> a -> f b@
--
-- Every 'Lens' is a valid 'Setter', choosing @f@ = 'Control.Lens.Getter.Mutator'.
--
-- Every 'Lens' can be used for 'Control.Lens.Getter.Getting' like a 'Control.Lens.Fold.Fold' that doesn't use
-- the 'Applicative' or 'Control.Lens.Getter.Gettable'.
--
-- Every 'Lens' is a valid 'Control.Lens.Traversal.Traversal' that only uses the 'Functor' part
-- of the 'Applicative' it is supplied.
--
-- Every 'Lens' can be used for 'Control.Lens.Getter.Getting' like a valid 'Getter', since 'Functor' is
-- a superclass of 'Control.Lens.Getter.Gettable'
--
-- Since every 'Lens' can be used for 'Control.Lens.Getter.Getting' like a valid 'Getter' it
-- follows that it must view exactly one element in the structure.
--
-- The lens laws follow from this property and the desire for it to act like
-- a 'Data.Traversable.Traversable' when used as a 'Control.Lens.Traversal.Traversal'.
----------------------------------------------------------------------------
module Control.Lens.Type
  (
  -- * Lenses
    Lens
  , Simple
  , lens
  , (%%~)
  , (%%=)

  -- * Traversing and Lensing
  , Focus(..)

  -- * Common Lenses
  -- ** Tuples
  , Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), Tuple5(..), Tuple6(..), Tuple7(..), Tuple8(..), Tuple9(..)
  -- ** Functions
  , resultAt

  -- * Lateral Composition
  , merged
  , bothLenses

  -- * Setting Functionally with Passthrough
  , (<%~), (<+~), (<-~), (<*~), (<//~), (<^~), (<^^~), (<**~), (<||~), (<&&~), (<<>~)

  -- * Setting State with Passthrough
  , (<%=), (<+=), (<-=), (<*=), (<//=), (<^=), (<^^=), (<**=), (<||=), (<&&=), (<<>=)

  -- * Cloning Lenses
  , clone

  -- * Simplified and In-Progress
  , LensLike
  , Overloaded
  , SimpleLens
  , SimpleLensLike
  , SimpleOverloaded

  -- * Tuple Fields
  , Field1, Field2, Field3, Field4, Field5, Field6, Field7, Field8, Field9

  -- * Tuple Field Substitutions
  , Subst1, Subst2, Subst3, Subst4, Subst5, Subst6, Subst7, Subst8, Subst9
  ) where

import Control.Applicative              as Applicative
import Control.Lens.Internal
import Control.Lens.Setter
import Control.Monad
import Control.Monad.State.Class        as State
import Control.Monad.Trans.State.Lazy   as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import Data.Monoid

infixr 4 %%~
infix  4 %%=
infixr 4 <+~, <*~, <-~, <//~, <^~, <^^~, <**~, <&&~, <||~, <%~, <<>~
infix  4 <+=, <*=, <-=, <//=, <^=, <^^=, <**=, <&&=, <||=, <%=, <<>=


--------------------------
-- Lenses
--------------------------

-- | A 'Lens' is actually a lens family as described in <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- With great power comes great responsibility and a 'Lens' is subject to the three common sense lens laws:
--
-- 1) You get back what you put in:
--
-- @'view' l ('set' l b a)  = b@
--
-- 2) Putting back what you got doesn't change anything:
--
-- @'set' l ('view' l a) a  = a@
--
-- 3) Setting twice is the same as setting once:
--
-- @'set' l c ('set' l b a) = 'set' l c a@
--
-- These laws are strong enough that the 4 type parameters of a 'Lens' cannot vary fully independently. For more on
-- how they interact, read the "Why is it a Lens Family?" section of <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- Every 'Lens' can be used directly as a 'Setter' or 'Control.Lens.Traversal.Traversal'.
--
-- You can also use a 'Lens' for 'Control.Lens.Getter.Getting' as if it were a 'Control.Lens.Fold.Fold' or 'Getter'.
--
-- Since every lens is a valid 'Control.Lens.Traversal.Traversal', the traversal laws should also apply to any lenses you create.
--
-- 1.) Idiomatic naturality:
--
-- @l 'pure' = 'pure'@
--
-- 2.) Sequential composition:
--
-- @'fmap' (l f) . l g = 'Data.Functor.Compose.getCompose' . l ('Data.Functor.Compose.Compose' . 'fmap' f . g)@
--
-- @type 'Lens' a b c d = forall f. 'Functor' f => 'LensLike' f a b c d@
type Lens a b c d = forall f. Functor f => (c -> f d) -> a -> f b

-- | A 'Simple' 'Lens', 'Simple' 'Control.Lens.Traversal.Traversal', ... can be used instead of a 'Lens','Control.Lens.Traversal.Traversal', ...
-- whenever the type variables don't change upon setting a value.
--
-- @
-- 'Data.Complex.Lens.imaginary' :: 'Simple' 'Lens' ('Data.Complex.Complex' a) a
-- 'Data.List.Lens.traverseHead' :: 'Simple' 'Control.Lens.Lens.Traversal' [a] a
-- @
--
-- Note: To use this alias in your own code with @'LensLike' f@ or 'Control.Lens.Setter.Setter', you may have to turn on
-- @LiberalTypeSynonyms@.
type Simple f a b = f a a b b

-- | @type 'SimpleLens' = 'Simple' 'Lens'@
type SimpleLens a b = Lens a a b b

-- | @type 'SimpleLensLike' f = 'Simple' ('LensLike' f)@
type SimpleLensLike f a b = LensLike f a a b b

--------------------------
-- Constructing Lenses
--------------------------

-- | Build a 'Lens' from a getter and a setter.
--
-- > lens :: Functor f => (a -> c) -> (a -> d -> b) -> (c -> f d) -> a -> f b
lens :: (a -> c) -> (a -> d -> b) -> Lens a b c d
lens ac adb cfd a = adb a <$> cfd (ac a)
{-# INLINE lens #-}

--------------------------
-- LensLike
--------------------------

-- |
-- Many combinators that accept a 'Lens' can also accept a 'Control.Lens.Traversal.Traversal' in limited situations.
--
-- They do so by specializing the type of 'Functor' that they require of the caller.
--
-- If a function accepts a @'LensLike' f a b c d@ for some 'Functor' @f@, then they may be passed a 'Lens'.
--
-- Further, if @f@ is an 'Applicative', they may also be passed a 'Control.Lens.Traversal.Traversal'.
type LensLike f a b c d = (c -> f d) -> a -> f b

-- | ('%%~') can be used in one of two scenarios:
--
-- When applied to a 'Lens', it can edit the target of the 'Lens' in a structure, extracting a
-- functorial result.
--
-- When applied to a 'Control.Lens.Traversal.Traversal', it can edit the targets of the 'Traversals', extracting an
-- applicative summary of its actions.
--
-- For all that the definition of this combinator is just:
--
-- @('%%~') = 'id'@
--
-- @
-- (%%~) :: 'Functor' f =>     'Control.Lens.Iso.Iso' a b c d       -> (c -> f d) -> a -> f b
-- (%%~) :: 'Functor' f =>     'Lens' a b c d      -> (c -> f d) -> a -> f b
-- (%%~) :: 'Applicative' f => 'Control.Lens.Traversal.Traversal' a b c d -> (c -> f d) -> a -> f b
-- @
--
-- It may be beneficial to think about it as if it had these even more restrictive types, however:
--
-- When applied to a 'Control.Lens.Traversal.Traversal', it can edit the targets of the 'Traversals', extracting a
-- supplemental monoidal summary of its actions, by choosing f = ((,) m)
--
-- @
-- (%%~) ::             'Control.Lens.Iso.Iso' a b c d       -> (c -> (e, d)) -> a -> (e, b)
-- (%%~) ::             'Lens' a b c d      -> (c -> (e, d)) -> a -> (e, b)
-- (%%~) :: 'Monoid' m => 'Control.Lens.Traversal.Traversal' a b c d -> (c -> (m, d)) -> a -> (m, b)
-- @
(%%~) :: LensLike f a b c d -> (c -> f d) -> a -> f b
(%%~) = id
{-# INLINE (%%~) #-}

-- | Modify the target of a 'Lens' in the current state returning some extra information of @c@ or
-- modify all targets of a 'Control.Lens.Traversal.Traversal' in the current state, extracting extra information of type @c@
-- and return a monoidal summary of the changes.
--
-- @('%%=') = ('state' '.')@
--
-- It may be useful to think of ('%%='), instead, as having either of the following more restricted
-- type signatures:
--
-- @
-- (%%=) :: 'MonadState' a m             => 'Control.Lens.Iso.Iso' a a c d       -> (c -> (e, d) -> m e
-- (%%=) :: 'MonadState' a m             => 'Lens' a a c d      -> (c -> (e, d) -> m e
-- (%%=) :: ('MonadState' a m, 'Monoid' e) => 'Control.Lens.Traversal.Traversal' a a c d -> (c -> (e, d) -> m e
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

-- | This class allows us to use 'focus' on a number of different monad transformers.
class Focus st where
  -- | Run a monadic action in a larger context than it was defined in, using a 'Simple' 'Lens' or 'Simple' 'Control.Lens.Traversal.Traversal'.
  --
  -- This is commonly used to lift actions in a simpler state monad into a state monad with a larger state type.
  --
  -- When applied to a 'Simple 'Control.Lens.Traversal.Traversal' over multiple values, the actions for each target are executed sequentially
  -- and the results are aggregated monoidally
  -- and a monoidal summary
  -- of the result is given.
  --
  -- @
  -- focus :: 'Monad' m             => 'Simple' 'Control.Lens.Iso.Iso' a b       -> st b m c -> st a m c
  -- focus :: 'Monad' m             => 'Simple' 'Lens' a b      -> st b m c -> st a m c
  -- focus :: ('Monad' m, 'Monoid' c) => 'Simple' 'Control.Lens.Traversal.Traversal' a b -> st b m c -> st a m c
  -- @
  focus :: Monad m => LensLike (Focusing m c) a a b b -> st b m c -> st a m c

  -- | Like 'focus', but discarding any accumulated results as you go.
  --
  -- @
  -- focus_ :: 'Monad' m             => 'Simple' 'Control.Lens.Iso.Iso' a b       -> st b m c -> st a m ()
  -- focus_ :: 'Monad' m             => 'Simple' 'Lens' a b      -> st b m c -> st a m ()
  -- focus_ :: ('Monad' m, 'Monoid' c) => 'Simple' 'Control.Lens.Traversal.Traversal' a b -> st b m c -> st a m ()
  -- @
  focus_ :: Monad m => LensLike (Focusing m ()) a a b b -> st b m c -> st a m ()

  -- | A much more limited version of 'focus' that can work with a 'Setter'.
  setFocus :: Simple Setter a b -> st b Identity c -> st a Identity ()

skip :: a -> ()
skip _ = ()
{-# INLINE skip #-}

instance Focus Strict.StateT where
  focus l m = Strict.StateT $ unfocusing . l (Focusing . Strict.runStateT m)
  {-# INLINE focus #-}
  focus_ l m = Strict.StateT $ unfocusing . l (Focusing . Strict.runStateT (liftM skip m))
  {-# INLINE focus_ #-}
  setFocus l m = Strict.state $ (,) () . runIdentity . l (Identity . snd . Strict.runState m)

instance Focus Lazy.StateT where
  focus l m = Lazy.StateT $ unfocusing . l (Focusing . Lazy.runStateT m)
  {-# INLINE focus #-}
  focus_ l m = Lazy.StateT $ unfocusing . l (Focusing . Lazy.runStateT (liftM skip m))
  {-# INLINE focus_ #-}
  setFocus l m = Lazy.state $ (,) () . runIdentity . l (Identity . snd . Lazy.runState m)

instance Focus ReaderT where
  focus l m = ReaderT $ liftM fst . unfocusing . l (\b -> Focusing $ (\c -> (c,b)) `liftM` runReaderT m b)
  {-# INLINE focus #-}
  focus_ l m = ReaderT $ \a -> liftM skip $ unfocusing $ l (\b -> Focusing $ (\_ -> ((),b)) `liftM` runReaderT m b) a
  {-# INLINE focus_ #-}
  setFocus _ _ = return () -- BOOORING

------------------------------------------------------------------------------
-- Common Lenses
------------------------------------------------------------------------------

{-
-- | This is a lens that can change the value (and type) of the first field of
-- a pair.
--
-- >>> import Control.Lens
-- >>> (1,2)^._1
-- 1
--
-- >>> _1 .~ "hello" $ (1,2)
-- ("hello",2)
--
-- @_1 :: 'Functor' f => (a -> f b) -> (a,c) -> f (a,c)@
_1 :: Lens (a,c) (b,c) a b
_1 f (a,c) = (\b -> (b,c)) <$> f a
{-# INLINE _1 #-}

-- | As '_1', but for the second field of a pair.
--
-- @
-- 'Control.Lens.Fold.anyOf' '_2' :: (c -> 'Bool') -> (a, c) -> 'Bool'
-- 'Data.Traversable.traverse' '.' '_2' :: ('Applicative' f, 'Data.Traversable.Traversable' t) => (a -> f b) -> t (c, a) -> f (t (c, b))
-- 'Control.Lens.Fold.foldMapOf' ('Data.Traversable.traverse' '.' '_2') :: ('Data.Traversable.Traversable' t, 'Data.Monoid.Monoid' m) => (c -> m) -> t (b, c) -> m
-- @
--
-- @_2 :: 'Functor' f => (a -> f b) -> (c,a) -> f (c,b)@
_2 :: Lens (c,a) (c,b) a b
_2 f (c,a) = (,) c <$> f a
{-# INLINE _2 #-}
-}

-- | This lens can be used to change the result of a function but only where
-- the arguments match the key given.
resultAt :: Eq e => e -> Simple Lens (e -> a) a
resultAt e afa ea = go <$> afa a where
  a = ea e
  go a' e' | e == e'   = a'
           | otherwise = a
{-# INLINE resultAt #-}

-- | Merge two lenses, getters, setters, folds or traversals.
merged :: Functor f => LensLike f a b c c -> LensLike f a' b' c c -> LensLike f (Either a a') (Either b b') c c
merged l _ f (Left a)   = Left <$> l f a
merged _ r f (Right a') = Right <$> r f a'
{-# INLINE merged #-}

-- | 'bothLenses' makes a 'Lens' from two other lenses (or isomorphisms)
bothLenses :: Lens a b c d -> Lens a' b' c' d' -> Lens (a,a') (b,b') (c,c') (d,d')
bothLenses l r f (a, a') = case l (IndexedStore id) a of
  IndexedStore db c -> case r (IndexedStore id) a' of
    IndexedStore db' c' -> (\(d,d') -> (db d, db' d')) <$> f (c,c')
{-# INLINE bothLenses #-}

------------------------------------------------------------------------------
-- Cloning Lenses
------------------------------------------------------------------------------

-- |
--
-- Cloning a 'Lens' is one way to make sure you arent given
-- something weaker, such as a 'Control.Lens.Traversal.Traversal' and can be used
-- as a way to pass around lenses that have to be monomorphic in 'f'.
--
-- Note: This only accepts a proper 'Lens', because 'IndexedStore' lacks its
-- (admissable) 'Applicative' instance.
--
-- \"Costate Comonad Coalgebra is equivalent of Java's member variable update technology for Haskell\"
-- -- \@PLT_Borat on Twitter
clone :: Functor f
      => LensLike (IndexedStore c d) a b c d
      -> (c -> f d) -> a -> f b
clone f cfd a = case f (IndexedStore id) a of
  IndexedStore db c -> db <$> cfd c
{-# INLINE clone #-}

-----------------------------------------------------------------------------
-- Overloading function application
-----------------------------------------------------------------------------

-- | @type 'LensLike' f a b c d = 'Overloaded' (->) f a b c d@
type Overloaded k f a b c d = k (c -> f d) (a -> f b)

-- | @type 'SimpleOverloaded' k f a b = 'Simple' ('Overloaded' k f) a b@
type SimpleOverloaded k f a b = Overloaded k f a a b b

-----------------------------------------------------------------------------
-- Setting and Remembering
-----------------------------------------------------------------------------

-- | Modify the target of a 'Lens' and return the result
--
-- When you do not need the result of the addition, ('+~') is more flexible.
(<%~) :: LensLike ((,)d) a b c d -> (c -> d) -> a -> (d, b)
l <%~ f = l $ \c -> let d = f c in (d, d)
{-# INLINE (<%~) #-}

-- | Increment the target of a numerically valued 'Lens' and return the result
--
-- When you do not need the result of the addition, ('+~') is more flexible.
(<+~) :: Num c => LensLike ((,)c) a b c c -> c -> a -> (c, b)
l <+~ c = l <%~ (+ c)
{-# INLINE (<+~) #-}

-- | Decrement the target of a numerically valued 'Lens' and return the result
--
-- When you do not need the result of the subtraction, ('-~') is more flexible.
(<-~) :: Num c => LensLike ((,)c) a b c c -> c -> a -> (c, b)
l <-~ c = l <%~ subtract c
{-# INLINE (<-~) #-}

-- | Multiply the target of a numerically valued 'Lens' and return the result
--
-- When you do not need the result of the multiplication, ('*~') is more flexible.
(<*~) :: Num c => LensLike ((,)c) a b c c -> c -> a -> (c, b)
l <*~ c = l <%~ (* c)
{-# INLINE (<*~) #-}

-- | Divide the target of a fractionally valued 'Lens' and return the result.
--
-- When you do not need the result of the division, ('//~') is more flexible.
(<//~) :: Fractional c => LensLike ((,)c) a b c c -> c -> a -> (c, b)
l <//~ c = l <%~ (/ c)
{-# INLINE (<//~) #-}

-- | Raise the target of a numerically valued 'Lens' to a non-negative 'Integral' power and return the result
--
-- When you do not need the result of the division, ('^~') is more flexible.
(<^~) :: (Num c, Integral d) => LensLike ((,)c) a b c c -> d -> a -> (c, b)
l <^~ d = l <%~ (^ d)
{-# INLINE (<^~) #-}

-- | Raise the target of a fractionally valued 'Lens' to an 'Integral' power and return the result
--
-- When you do not need the result of the division, ('^^~') is more flexible.
(<^^~) :: (Fractional c, Integral d) => LensLike ((,)c) a b c c -> d -> a -> (c, b)
l <^^~ d = l <%~ (^^ d)
{-# INLINE (<^^~) #-}

-- | Raise the target of a floating-point valued 'Lens' to an arbitrary power and return the result
--
-- When you do not need the result of the division, ('**~') is more flexible.
(<**~) :: Floating c => LensLike ((,)c) a b c c -> c -> a -> (c, b)
l <**~ c = l <%~ (** c)
{-# INLINE (<**~) #-}

-- | Logically '||' a Boolean valued 'Lens' and return the result
--
-- When you do not need the result of the operation, ('||~') is more flexible.
(<||~) :: LensLike ((,)Bool) a b Bool Bool -> Bool -> a -> (Bool, b)
l <||~ c = l <%~ (|| c)
{-# INLINE (<||~) #-}

-- | Logically '&&' a Boolean valued 'Lens' and return the result
--
-- When you do not need the result of the operation, ('&&~') is more flexible.
(<&&~) :: LensLike ((,)Bool) a b Bool Bool -> Bool -> a -> (Bool, b)
l <&&~ c = l <%~ (&& c)
{-# INLINE (<&&~) #-}

-- | 'mappend' a monoidal value onto the end of the target of a 'Lens' and return the result
--
-- When you do not need the result of the operation, ('<>~') is more flexible.
(<<>~) :: Monoid m => LensLike ((,)m) a b m m -> m -> a -> (m, b)
l <<>~ m = l <%~ (`mappend` m)
{-# INLINE (<<>~) #-}

-----------------------------------------------------------------------------
-- Setting and Remembering State
-----------------------------------------------------------------------------

-- | Modify the target of a 'Lens' into your monad's state by a user supplied function and return the result.
--
-- When you do not need the result of the operation, ('%=') is more flexible.
(<%=) :: MonadState a m => LensLike ((,)d) a a c d -> (c -> d) -> m d
l <%= f = l %%= (\c -> let d = f c in (d,d))
{-# INLINE (<%=) #-}

-- | Add to the target of a numerically valued 'Lens' into your monad's state and return the result.
--
-- When you do not need the result of the multiplication, ('+=') is more flexible.
(<+=) :: (MonadState a m, Num b) => SimpleLensLike ((,)b) a b -> b -> m b
l <+= b = l <%= (+ b)
{-# INLINE (<+=) #-}

-- | Subtract from the target of a numerically valued 'Lens' into your monad's state and return the result.
--
-- When you do not need the result of the multiplication, ('-=') is more flexible.
(<-=) :: (MonadState a m, Num b) => SimpleLensLike ((,)b) a b -> b -> m b
l <-= b = l <%= subtract b
{-# INLINE (<-=) #-}

-- | Multiply the target of a numerically valued 'Lens' into your monad's state and return the result.
--
-- When you do not need the result of the multiplication, ('*=') is more flexible.
(<*=) :: (MonadState a m, Num b) => SimpleLensLike ((,)b) a b -> b -> m b
l <*= b = l <%= (* b)
{-# INLINE (<*=) #-}

-- | Divide the target of a fractionally valued 'Lens' into your monad's state and return the result.
--
-- When you do not need the result of the division, ('//=') is more flexible.
(<//=) :: (MonadState a m, Fractional b) => SimpleLensLike ((,)b) a b -> b -> m b
l <//= b = l <%= (/ b)
{-# INLINE (<//=) #-}

-- | Raise the target of a numerically valued 'Lens' into your monad's state to a non-negative 'Integral' power and return the result
--
-- When you do not need the result of the operation, ('**=') is more flexible.
(<^=) :: (MonadState a m, Num b, Integral c) => SimpleLensLike ((,)b) a b -> c -> m b
l <^= c = l <%= (^ c)
{-# INLINE (<^=) #-}

-- | Raise the target of a fractionally valued 'Lens' into your monad's state to an 'Integral' power and return the result
--
-- When you do not need the result of the operation, ('^^=') is more flexible.
(<^^=) :: (MonadState a m, Fractional b, Integral c) => SimpleLensLike ((,)b) a b -> c -> m b
l <^^= c = l <%= (^^ c)
{-# INLINE (<^^=) #-}

-- | Raise the target of a floating-point valued 'Lens' into your monad's state to an arbitrary power and return the result
--
-- When you do not need the result of the operation, ('**=') is more flexible.
(<**=) :: (MonadState a m, Floating b) => SimpleLensLike ((,)b) a b -> b -> m b
l <**= b = l <%= (** b)
{-# INLINE (<**=) #-}

-- | Logically '||' a Boolean valued 'Lens' into your monad's state and return the result
--
-- When you do not need the result of the operation, ('||=') is more flexible.
(<||=) :: MonadState a m => SimpleLensLike ((,)Bool) a Bool -> Bool -> m Bool
l <||= b = l <%= (|| b)
{-# INLINE (<||=) #-}

-- | Logically '&&' a Boolean valued 'Lens' into your monad's state and return the result
--
-- When you do not need the result of the operation, ('&&=') is more flexible.
(<&&=) :: MonadState a m => SimpleLensLike ((,)Bool) a Bool -> Bool -> m Bool
l <&&= b = l <%= (&& b)
{-# INLINE (<&&=) #-}

-- | 'mappend' a monoidal value onto the end of the target of a 'Lens' into your monad's state and return the result
--
-- When you do not need the result of the operation, ('<>=') is more flexible.
(<<>=) :: (MonadState a m, Monoid r) => SimpleLensLike ((,)r) a r -> r -> m r
l <<>= r = l <%= (`mappend` r)
{-# INLINE (<<>=) #-}

-- | Access the 1st field of a tuple
--
-- @
-- type instance Field1 (a,b) = a
-- type instance Field1 (a,b,c) = a
-- type instance Field1 (a,b,c,d) = a
-- type instance Field1 (a,b,c,d,e) = a
-- type instance Field1 (a,b,c,d,e,f) = a
-- type instance Field1 (a,b,c,d,e,f,g) = a
-- type instance Field1 (a,b,c,d,e,f,g,h) = a
-- type instance Field1 (a,b,c,d,e,f,g,h,i) = a
-- @
type family Field1 a
type instance Field1 (a,b) = a
type instance Field1 (a,b,c) = a
type instance Field1 (a,b,c,d) = a
type instance Field1 (a,b,c,d,e) = a
type instance Field1 (a,b,c,d,e,f) = a
type instance Field1 (a,b,c,d,e,f,g) = a
type instance Field1 (a,b,c,d,e,f,g,h) = a
type instance Field1 (a,b,c,d,e,f,g,h,i) = a

-- | Change the type of the 1st field in a tuple
--
-- @
-- type instance Subst1 (a,b) a' = (a',b)
-- type instance Subst1 (a,b,c) a' = (a',b,c)
-- type instance Subst1 (a,b,c,d) a' = (a',b,c,d)
-- type instance Subst1 (a,b,c,d,e) a' = (a',b,c,d,e)
-- type instance Subst1 (a,b,c,d,e,f) a' = (a',b,c,d,e,f)
-- type instance Subst1 (a,b,c,d,e,f,g) a' = (a',b,c,d,e,f,g)
-- type instance Subst1 (a,b,c,d,e,f,g,h) a' = (a',b,c,d,e,f,g,h)
-- type instance Subst1 (a,b,c,d,e,f,g,h,i) a' = (a',b,c,d,e,f,g,h,i)
-- @
type family Subst1 a b
type instance Subst1 (a,b) a' = (a',b)
type instance Subst1 (a,b,c) a' = (a',b,c)
type instance Subst1 (a,b,c,d) a' = (a',b,c,d)
type instance Subst1 (a,b,c,d,e) a' = (a',b,c,d,e)
type instance Subst1 (a,b,c,d,e,f) a' = (a',b,c,d,e,f)
type instance Subst1 (a,b,c,d,e,f,g) a' = (a',b,c,d,e,f,g)
type instance Subst1 (a,b,c,d,e,f,g,h) a' = (a',b,c,d,e,f,g,h)
type instance Subst1 (a,b,c,d,e,f,g,h,i) a' = (a',b,c,d,e,f,g,h,i)

-- | Provides access to 1st field of a tuple.
class (Subst1 a (Field1 b) ~ b, Subst1 b (Field1 a) ~ a) => Tuple1 a b where
  -- | Access the 1st field of a tuple (and possibly change its type).
  --
  -- >>> import Control.Lens
  -- >>> (1,2)^._1
  -- 1
  --
  -- >>> _1 .~ "hello" $ (1,2)
  -- ("hello",2)
  --
  -- This can also be used on larger tuples as well
  --
  -- >>> _1 +~ 41 $ (1,2,3,4,5)
  -- (42,2,3,4,5)
  --
  -- @
  -- _1 :: 'Lens' (a,b) (a',b) a a'
  -- _1 :: 'Lens' (a,b,c) (a',b,c) a a'
  -- _1 :: 'Lens' (a,b,c,d) (a',b,c,d) a a'
  -- ...
  -- _1 :: 'Lens' (a,c,d,e,f,g,h,i) (a',b,c,d,e,f,g,h,i) a a'
  -- @
  _1 :: Lens a b (Field1 a) (Field1 b)

instance Tuple1 (a,b) (a',b) where
  _1 k (a,b) = (\a' -> (a',b)) <$> k a
  {-# INLINE _1 #-}

instance Tuple1 (a,b,c) (a',b,c) where
  _1 k (a,b,c) = (\a' -> (a',b,c)) <$> k a
  {-# INLINE _1 #-}

instance Tuple1 (a,b,c,d) (a',b,c,d) where
  _1 k (a,b,c,d) = (\a' -> (a',b,c,d)) <$> k a
  {-# INLINE _1 #-}

instance Tuple1 (a,b,c,d,e) (a',b,c,d,e) where
  _1 k (a,b,c,d,e) = (\a' -> (a',b,c,d,e)) <$> k a
  {-# INLINE _1 #-}

instance Tuple1 (a,b,c,d,e,f) (a',b,c,d,e,f) where
  _1 k (a,b,c,d,e,f) = (\a' -> (a',b,c,d,e,f)) <$> k a
  {-# INLINE _1 #-}

instance Tuple1 (a,b,c,d,e,f,g) (a',b,c,d,e,f,g) where
  _1 k (a,b,c,d,e,f,g) = (\a' -> (a',b,c,d,e,f,g)) <$> k a
  {-# INLINE _1 #-}

instance Tuple1 (a,b,c,d,e,f,g,h) (a',b,c,d,e,f,g,h) where
  _1 k (a,b,c,d,e,f,g,h) = (\a' -> (a',b,c,d,e,f,g,h)) <$> k a
  {-# INLINE _1 #-}

instance Tuple1 (a,b,c,d,e,f,g,h,i) (a',b,c,d,e,f,g,h,i) where
  _1 k (a,b,c,d,e,f,g,h,i) = (\a' -> (a',b,c,d,e,f,g,h,i)) <$> k a
  {-# INLINE _1 #-}

-- | Extract the type of the 2nd field in a tuple
--
-- @
-- type instance Field2 (a,b) = b
-- type instance Field2 (a,b,c) = b
-- type instance Field2 (a,b,c,d) = b
-- type instance Field2 (a,b,c,d,e) = b
-- type instance Field2 (a,b,c,d,e,f) = b
-- type instance Field2 (a,b,c,d,e,f,g) = b
-- type instance Field2 (a,b,c,d,e,f,g,h) = b
-- type instance Field2 (a,b,c,d,e,f,g,h,i) = b
-- @
type family Field2 a
type instance Field2 (a,b) = b
type instance Field2 (a,b,c) = b
type instance Field2 (a,b,c,d) = b
type instance Field2 (a,b,c,d,e) = b
type instance Field2 (a,b,c,d,e,f) = b
type instance Field2 (a,b,c,d,e,f,g) = b
type instance Field2 (a,b,c,d,e,f,g,h) = b
type instance Field2 (a,b,c,d,e,f,g,h,i) = b

-- | Change the type of the 2nd field in a tuple
--
-- @
-- type instance Subst2 (a,b) b' = (a,b')
-- type instance Subst2 (a,b,c) b' = (a,b',c)
-- type instance Subst2 (a,b,c,d) b' = (a,b',c,d)
-- type instance Subst2 (a,b,c,d,e) b' = (a,b',c,d,e)
-- type instance Subst2 (a,b,c,d,e,f) b' = (a,b',c,d,e,f)
-- type instance Subst2 (a,b,c,d,e,f,g) b' = (a,b',c,d,e,f,g)
-- type instance Subst2 (a,b,c,d,e,f,g,h) b' = (a,b',c,d,e,f,g,h)
-- type instance Subst2 (a,b,c,d,e,f,g,h,i) b' = (a,b',c,d,e,f,g,h,i)
-- @
type family Subst2 a b
type instance Subst2 (a,b) b' = (a,b')
type instance Subst2 (a,b,c) b' = (a,b',c)
type instance Subst2 (a,b,c,d) b' = (a,b',c,d)
type instance Subst2 (a,b,c,d,e) b' = (a,b',c,d,e)
type instance Subst2 (a,b,c,d,e,f) b' = (a,b',c,d,e,f)
type instance Subst2 (a,b,c,d,e,f,g) b' = (a,b',c,d,e,f,g)
type instance Subst2 (a,b,c,d,e,f,g,h) b' = (a,b',c,d,e,f,g,h)
type instance Subst2 (a,b,c,d,e,f,g,h,i) b' = (a,b',c,d,e,f,g,h,i)

-- | Provides access to the 2nd field of a tuple
class (Subst2 a (Field2 b) ~ b, Subst2 b (Field2 a) ~ a, Tuple1 a a, Tuple1 b b) => Tuple2 a b where
  -- | Access the 2nd field of a tuple
  --
  -- >>> import Control.Lens
  -- >>> _2 .~ "hello" $ (1,(),3,4)
  -- (1,"hello",3,4)
  --
  -- @
  -- 'Control.Lens.Fold.anyOf' '_2' :: (c -> 'Bool') -> (a, c) -> 'Bool'
  -- 'Data.Traversable.traverse' '.' '_2' :: ('Applicative' f, 'Data.Traversable.Traversable' t) => (a -> f b) -> t (c, a) -> f (t (c, b))
  -- 'Control.Lens.Fold.foldMapOf' ('Data.Traversable.traverse' '.' '_2') :: ('Data.Traversable.Traversable' t, 'Data.Monoid.Monoid' m) => (c -> m) -> t (b, c) -> m
  -- @
  _2 :: Lens a b (Field2 a) (Field2 b)

instance Tuple2 (a,b) (a,b') where
  _2 k (a,b) = (\b' -> (a,b')) <$> k b
  {-# INLINE _2 #-}

instance Tuple2 (a,b,c) (a,b',c) where
  _2 k (a,b,c) = (\b' -> (a,b',c)) <$> k b
  {-# INLINE _2 #-}

instance Tuple2 (a,b,c,d) (a,b',c,d) where
  _2 k (a,b,c,d) = (\b' -> (a,b',c,d)) <$> k b
  {-# INLINE _2 #-}

instance Tuple2 (a,b,c,d,e) (a,b',c,d,e) where
  _2 k (a,b,c,d,e) = (\b' -> (a,b',c,d,e)) <$> k b
  {-# INLINE _2 #-}

instance Tuple2 (a,b,c,d,e,f) (a,b',c,d,e,f) where
  _2 k (a,b,c,d,e,f) = (\b' -> (a,b',c,d,e,f)) <$> k b
  {-# INLINE _2 #-}

instance Tuple2 (a,b,c,d,e,f,g) (a,b',c,d,e,f,g) where
  _2 k (a,b,c,d,e,f,g) = (\b' -> (a,b',c,d,e,f,g)) <$> k b
  {-# INLINE _2 #-}

instance Tuple2 (a,b,c,d,e,f,g,h) (a,b',c,d,e,f,g,h) where
  _2 k (a,b,c,d,e,f,g,h) = (\b' -> (a,b',c,d,e,f,g,h)) <$> k b
  {-# INLINE _2 #-}

instance Tuple2 (a,b,c,d,e,f,g,h,i) (a,b',c,d,e,f,g,h,i) where
  _2 k (a,b,c,d,e,f,g,h,i) = (\b' -> (a,b',c,d,e,f,g,h,i)) <$> k b
  {-# INLINE _2 #-}

-- | Extract the type of the 3rd field of a tuple
--
-- @
-- type instance Field3 (a,b,c) = c
-- type instance Field3 (a,b,c,d) = c
-- type instance Field3 (a,b,c,d,e) = c
-- type instance Field3 (a,b,c,d,e,f) = c
-- type instance Field3 (a,b,c,d,e,f,g) = c
-- type instance Field3 (a,b,c,d,e,f,g,h) = c
-- type instance Field3 (a,b,c,d,e,f,g,h,i) = c
-- @
type family Field3 a
type instance Field3 (a,b,c) = c
type instance Field3 (a,b,c,d) = c
type instance Field3 (a,b,c,d,e) = c
type instance Field3 (a,b,c,d,e,f) = c
type instance Field3 (a,b,c,d,e,f,g) = c
type instance Field3 (a,b,c,d,e,f,g,h) = c
type instance Field3 (a,b,c,d,e,f,g,h,i) = c

-- | Change the type of the 3rd field of a tuple
--
-- @
-- type instance Subst3 (a,b,c) c' = (a,b,c')
-- type instance Subst3 (a,b,c,d) c' = (a,b,c',d)
-- type instance Subst3 (a,b,c,d,e) c' = (a,b,c',d,e)
-- type instance Subst3 (a,b,c,d,e,f) c' = (a,b,c',d,e,f)
-- type instance Subst3 (a,b,c,d,e,f,g) c' = (a,b,c',d,e,f,g)
-- type instance Subst3 (a,b,c,d,e,f,g,h) c' = (a,b,c',d,e,f,g,h)
-- type instance Subst3 (a,b,c,d,e,f,g,h,i) c' = (a,b,c',d,e,f,g,h,i)
-- @
type family Subst3 a b
type instance Subst3 (a,b,c) c' = (a,b,c')
type instance Subst3 (a,b,c,d) c' = (a,b,c',d)
type instance Subst3 (a,b,c,d,e) c' = (a,b,c',d,e)
type instance Subst3 (a,b,c,d,e,f) c' = (a,b,c',d,e,f)
type instance Subst3 (a,b,c,d,e,f,g) c' = (a,b,c',d,e,f,g)
type instance Subst3 (a,b,c,d,e,f,g,h) c' = (a,b,c',d,e,f,g,h)
type instance Subst3 (a,b,c,d,e,f,g,h,i) c' = (a,b,c',d,e,f,g,h,i)

-- | Provides access to the 3rd field of a tuple
class (Subst3 a (Field3 b) ~ b, Subst3 b (Field3 a) ~ a, Tuple2 a a, Tuple2 b b) => Tuple3 a b where
  -- | Access the 3rd field of a tuple
  _3 :: Lens a b (Field3 a) (Field3 b)

instance Tuple3 (a,b,c) (a,b,c') where
  _3 k (a,b,c) = (\c' -> (a,b,c')) <$> k c
  {-# INLINE _3 #-}

instance Tuple3 (a,b,c,d) (a,b,c',d) where
  _3 k (a,b,c,d) = (\c' -> (a,b,c',d)) <$> k c
  {-# INLINE _3 #-}

instance Tuple3 (a,b,c,d,e) (a,b,c',d,e) where
  _3 k (a,b,c,d,e) = (\c' -> (a,b,c',d,e)) <$> k c
  {-# INLINE _3 #-}

instance Tuple3 (a,b,c,d,e,f) (a,b,c',d,e,f) where
  _3 k (a,b,c,d,e,f) = (\c' -> (a,b,c',d,e,f)) <$> k c
  {-# INLINE _3 #-}

instance Tuple3 (a,b,c,d,e,f,g) (a,b,c',d,e,f,g) where
  _3 k (a,b,c,d,e,f,g) = (\c' -> (a,b,c',d,e,f,g)) <$> k c
  {-# INLINE _3 #-}

instance Tuple3 (a,b,c,d,e,f,g,h) (a,b,c',d,e,f,g,h) where
  _3 k (a,b,c,d,e,f,g,h) = (\c' -> (a,b,c',d,e,f,g,h)) <$> k c
  {-# INLINE _3 #-}

instance Tuple3 (a,b,c,d,e,f,g,h,i) (a,b,c',d,e,f,g,h,i) where
  _3 k (a,b,c,d,e,f,g,h,i) = (\c' -> (a,b,c',d,e,f,g,h,i)) <$> k c
  {-# INLINE _3 #-}

-- | Extract the type of the 4th field of a tuple.
--
-- @
-- type instance Field4 (a,b,c,d) = d
-- type instance Field4 (a,b,c,d,e) = d
-- type instance Field4 (a,b,c,d,e,f) = d
-- type instance Field4 (a,b,c,d,e,f,g) = d
-- type instance Field4 (a,b,c,d,e,f,g,h) = d
-- type instance Field4 (a,b,c,d,e,f,g,h,i) = d
-- @
type family Field4 a
type instance Field4 (a,b,c,d) = d
type instance Field4 (a,b,c,d,e) = d
type instance Field4 (a,b,c,d,e,f) = d
type instance Field4 (a,b,c,d,e,f,g) = d
type instance Field4 (a,b,c,d,e,f,g,h) = d
type instance Field4 (a,b,c,d,e,f,g,h,i) = d

-- | Change the type of the 4th field of a tuple
--
-- @
-- type instance Subst4 (a,b,c,d) d' = (a,b,c,d')
-- type instance Subst4 (a,b,c,d,e) d' = (a,b,c,d',e)
-- type instance Subst4 (a,b,c,d,e,f) d' = (a,b,c,d',e,f)
-- type instance Subst4 (a,b,c,d,e,f,g) d' = (a,b,c,d',e,f,g)
-- type instance Subst4 (a,b,c,d,e,f,g,h) d' = (a,b,c,d',e,f,g,h)
-- type instance Subst4 (a,b,c,d,e,f,g,h,i) d' = (a,b,c,d',e,f,g,h,i)
-- @
type family Subst4 a b
type instance Subst4 (a,b,c,d) d' = (a,b,c,d')
type instance Subst4 (a,b,c,d,e) d' = (a,b,c,d',e)
type instance Subst4 (a,b,c,d,e,f) d' = (a,b,c,d',e,f)
type instance Subst4 (a,b,c,d,e,f,g) d' = (a,b,c,d',e,f,g)
type instance Subst4 (a,b,c,d,e,f,g,h) d' = (a,b,c,d',e,f,g,h)
type instance Subst4 (a,b,c,d,e,f,g,h,i) d' = (a,b,c,d',e,f,g,h,i)

-- | Provide access to the 4th field of a tuple
class (Subst4 a (Field4 b) ~ b, Subst4 b (Field4 a) ~ a, Tuple3 a a, Tuple3 b b) => Tuple4 a b where
  -- | Access the 4th field of a tuple
  _4 :: Lens a b (Field4 a) (Field4 b)

instance Tuple4 (a,b,c,d) (a,b,c,d') where
  _4 k (a,b,c,d) = (\d' -> (a,b,c,d')) <$> k d
  {-# INLINE _4 #-}

instance Tuple4 (a,b,c,d,e) (a,b,c,d',e) where
  _4 k (a,b,c,d,e) = (\d' -> (a,b,c,d',e)) <$> k d
  {-# INLINE _4 #-}

instance Tuple4 (a,b,c,d,e,f) (a,b,c,d',e,f) where
  _4 k (a,b,c,d,e,f) = (\d' -> (a,b,c,d',e,f)) <$> k d
  {-# INLINE _4 #-}

instance Tuple4 (a,b,c,d,e,f,g) (a,b,c,d',e,f,g) where
  _4 k (a,b,c,d,e,f,g) = (\d' -> (a,b,c,d',e,f,g)) <$> k d
  {-# INLINE _4 #-}

instance Tuple4 (a,b,c,d,e,f,g,h) (a,b,c,d',e,f,g,h) where
  _4 k (a,b,c,d,e,f,g,h) = (\d' -> (a,b,c,d',e,f,g,h)) <$> k d
  {-# INLINE _4 #-}

instance Tuple4 (a,b,c,d,e,f,g,h,i) (a,b,c,d',e,f,g,h,i) where
  _4 k (a,b,c,d,e,f,g,h,i) = (\d' -> (a,b,c,d',e,f,g,h,i)) <$> k d
  {-# INLINE _4 #-}

-- | Extract the type of the 5th field of a tuple.
--
-- @
-- type instance Field5 (a,b,c,d,e) = e
-- type instance Field5 (a,b,c,d,e,f) = e
-- type instance Field5 (a,b,c,d,e,f,g) = e
-- type instance Field5 (a,b,c,d,e,f,g,h) = e
-- type instance Field5 (a,b,c,d,e,f,g,h,i) = e
-- @
type family Field5 a
type instance Field5 (a,b,c,d,e) = e
type instance Field5 (a,b,c,d,e,f) = e
type instance Field5 (a,b,c,d,e,f,g) = e
type instance Field5 (a,b,c,d,e,f,g,h) = e
type instance Field5 (a,b,c,d,e,f,g,h,i) = e

-- | Change the type of the 5th field of a tuple.
--
-- @
-- type instance Subst5 (a,b,c,d,e) e' = (a,b,c,d,e')
-- type instance Subst5 (a,b,c,d,e,f) e' = (a,b,c,d,e',f)
-- type instance Subst5 (a,b,c,d,e,f,g) e' = (a,b,c,d,e',f,g)
-- type instance Subst5 (a,b,c,d,e,f,g,h) e' = (a,b,c,d,e',f,g,h)
-- type instance Subst5 (a,b,c,d,e,f,g,h,i) e' = (a,b,c,d,e',f,g,h,i)
-- @
type family Subst5 a b
type instance Subst5 (a,b,c,d,e) e' = (a,b,c,d,e')
type instance Subst5 (a,b,c,d,e,f) e' = (a,b,c,d,e',f)
type instance Subst5 (a,b,c,d,e,f,g) e' = (a,b,c,d,e',f,g)
type instance Subst5 (a,b,c,d,e,f,g,h) e' = (a,b,c,d,e',f,g,h)
type instance Subst5 (a,b,c,d,e,f,g,h,i) e' = (a,b,c,d,e',f,g,h,i)

-- | Provides access to the 5th field of a tuple
class (Subst5 a (Field5 b) ~ b, Subst5 b (Field5 a) ~ a, Tuple4 a a, Tuple4 b b) => Tuple5 a b where
  -- | Access the 5th field of a tuple
  _5 :: Lens a b (Field5 a) (Field5 b)

instance Tuple5 (a,b,c,d,e) (a,b,c,d,e') where
  _5 k (a,b,c,d,e) = (\e' -> (a,b,c,d,e')) <$> k e
  {-# INLINE _5 #-}

instance Tuple5 (a,b,c,d,e,f) (a,b,c,d,e',f) where
  _5 k (a,b,c,d,e,f) = (\e' -> (a,b,c,d,e',f)) <$> k e
  {-# INLINE _5 #-}

instance Tuple5 (a,b,c,d,e,f,g) (a,b,c,d,e',f,g) where
  _5 k (a,b,c,d,e,f,g) = (\e' -> (a,b,c,d,e',f,g)) <$> k e
  {-# INLINE _5 #-}

instance Tuple5 (a,b,c,d,e,f,g,h) (a,b,c,d,e',f,g,h) where
  _5 k (a,b,c,d,e,f,g,h) = (\e' -> (a,b,c,d,e',f,g,h)) <$> k e
  {-# INLINE _5 #-}

instance Tuple5 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e',f,g,h,i) where
  _5 k (a,b,c,d,e,f,g,h,i) = (\e' -> (a,b,c,d,e',f,g,h,i)) <$> k e
  {-# INLINE _5 #-}

-- | Extract the type of the 6th field of a tuple.
--
-- @
-- type instance Field6 (a,b,c,d,e,f) = f
-- type instance Field6 (a,b,c,d,e,f,g) = f
-- type instance Field6 (a,b,c,d,e,f,g,h) = f
-- type instance Field6 (a,b,c,d,e,f,g,h,i) = f
-- @
type family Field6 a
type instance Field6 (a,b,c,d,e,f) = f
type instance Field6 (a,b,c,d,e,f,g) = f
type instance Field6 (a,b,c,d,e,f,g,h) = f
type instance Field6 (a,b,c,d,e,f,g,h,i) = f

-- | Change the type of the 6th field of a tuple.
--
-- @
-- type instance Subst6 (a,b,c,d,e,f) f' = (a,b,c,d,e,f')
-- type instance Subst6 (a,b,c,d,e,f,g) f' = (a,b,c,d,e,f',g)
-- type instance Subst6 (a,b,c,d,e,f,g,h) f' = (a,b,c,d,e,f',g,h)
-- type instance Subst6 (a,b,c,d,e,f,g,h,i) f' = (a,b,c,d,e,f',g,h,i)
-- @
type family Subst6 a b
type instance Subst6 (a,b,c,d,e,f) f' = (a,b,c,d,e,f')
type instance Subst6 (a,b,c,d,e,f,g) f' = (a,b,c,d,e,f',g)
type instance Subst6 (a,b,c,d,e,f,g,h) f' = (a,b,c,d,e,f',g,h)
type instance Subst6 (a,b,c,d,e,f,g,h,i) f' = (a,b,c,d,e,f',g,h,i)

-- | Provides access to the 6th element of a tuple
class (Subst6 a (Field6 b) ~ b, Subst6 b (Field6 a) ~ a, Tuple5 a a, Tuple5 b b) => Tuple6 a b where
  -- | Access the 6th field of a tuple
  _6 :: Lens a b (Field6 a) (Field6 b)

instance Tuple6 (a,b,c,d,e,f) (a,b,c,d,e,f') where
  _6 k (a,b,c,d,e,f) = (\f' -> (a,b,c,d,e,f')) <$> k f
  {-# INLINE _6 #-}

instance Tuple6 (a,b,c,d,e,f,g) (a,b,c,d,e,f',g) where
  _6 k (a,b,c,d,e,f,g) = (\f' -> (a,b,c,d,e,f',g)) <$> k f
  {-# INLINE _6 #-}

instance Tuple6 (a,b,c,d,e,f,g,h) (a,b,c,d,e,f',g,h) where
  _6 k (a,b,c,d,e,f,g,h) = (\f' -> (a,b,c,d,e,f',g,h)) <$> k f
  {-# INLINE _6 #-}

instance Tuple6 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f',g,h,i) where
  _6 k (a,b,c,d,e,f,g,h,i) = (\f' -> (a,b,c,d,e,f',g,h,i)) <$> k f
  {-# INLINE _6 #-}

-- | Extract the type of the 7th field of a tuple
--
-- @
-- type instance Field7 (a,b,c,d,e,f,g) = g
-- type instance Field7 (a,b,c,d,e,f,g,h) = g
-- type instance Field7 (a,b,c,d,e,f,g,h,i) = g
-- @
type family Field7 a
type instance Field7 (a,b,c,d,e,f,g) = g
type instance Field7 (a,b,c,d,e,f,g,h) = g
type instance Field7 (a,b,c,d,e,f,g,h,i) = g

-- | Change the type of the 7th field of a tuple
--
-- @
-- type instance Subst7 (a,b,c,d,e,f,g) g' = (a,b,c,d,e,f,g')
-- type instance Subst7 (a,b,c,d,e,f,g,h) g' = (a,b,c,d,e,f,g',h)
-- type instance Subst7 (a,b,c,d,e,f,g,h,i) g' = (a,b,c,d,e,f,g',h,i)
-- @
type family Subst7 a b
type instance Subst7 (a,b,c,d,e,f,g) g' = (a,b,c,d,e,f,g')
type instance Subst7 (a,b,c,d,e,f,g,h) g' = (a,b,c,d,e,f,g',h)
type instance Subst7 (a,b,c,d,e,f,g,h,i) g' = (a,b,c,d,e,f,g',h,i)

-- | Provide access to the 7th field of a tuple
class (Subst7 a (Field7 b) ~ b, Subst7 b (Field7 a) ~ a, Tuple6 a a, Tuple6 b b) => Tuple7 a b where
  -- | Access the 7th field of a tuple
  _7 :: Lens a b (Field7 a) (Field7 b)

instance Tuple7 (a,b,c,d,e,f,g) (a,b,c,d,e,f,g') where
  _7 k (a,b,c,d,e,f,g) = (\g' -> (a,b,c,d,e,f,g')) <$> k g
  {-# INLINE _7 #-}

instance Tuple7 (a,b,c,d,e,f,g,h) (a,b,c,d,e,f,g',h) where
  _7 k (a,b,c,d,e,f,g,h) = (\g' -> (a,b,c,d,e,f,g',h)) <$> k g
  {-# INLINE _7 #-}

instance Tuple7 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f,g',h,i) where
  _7 k (a,b,c,d,e,f,g,h,i) = (\g' -> (a,b,c,d,e,f,g',h,i)) <$> k g
  {-# INLINE _7 #-}

-- | Extract the type of the 8th field of a tuple
--
-- @
-- type instance Field8 (a,b,c,d,e,f,g,h) = h
-- type instance Field8 (a,b,c,d,e,f,g,h,i) = h
-- @
type family Field8 a
type instance Field8 (a,b,c,d,e,f,g,h) = h
type instance Field8 (a,b,c,d,e,f,g,h,i) = h

-- | Change the type of the 8th field of a tuple
--
-- @
-- type instance Subst8 (a,b,c,d,e,f,g,h) h' = (a,b,c,d,e,f,g,h')
-- type instance Subst8 (a,b,c,d,e,f,g,h,i) h' = (a,b,c,d,e,f,g,h',i)
-- @
type family Subst8 a b
type instance Subst8 (a,b,c,d,e,f,g,h) h' = (a,b,c,d,e,f,g,h')
type instance Subst8 (a,b,c,d,e,f,g,h,i) h' = (a,b,c,d,e,f,g,h',i)

-- | Provide access to the 8th field of a tuple
class (Subst8 a (Field8 b) ~ b, Subst8 b (Field8 a) ~ a, Tuple7 a a, Tuple7 b b) => Tuple8 a b where
  -- | Access the 8th field of a tuple
  _8 :: Lens a b (Field8 a) (Field8 b)

instance Tuple8 (a,b,c,d,e,f,g,h) (a,b,c,d,e,f,g,h') where
  _8 k (a,b,c,d,e,f,g,h) = (\h' -> (a,b,c,d,e,f,g,h')) <$> k h
  {-# INLINE _8 #-}

instance Tuple8 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f,g,h',i) where
  _8 k (a,b,c,d,e,f,g,h,i) = (\h' -> (a,b,c,d,e,f,g,h',i)) <$> k h
  {-# INLINE _8 #-}

-- | Extract the type of the 9th field of the tuple
--
-- @
-- type instance Field9 (a,b,c,d,e,f,g,h,i) = i
-- @
type family Field9 a
type instance Field9 (a,b,c,d,e,f,g,h,i) = i

-- | Change the type of the 9th field of a tuple
--
-- @
-- type instance Subst9 (a,b,c,d,e,f,g,h,i) i' = (a,b,c,d,e,f,g,h,i')
-- @
type family Subst9 a b
type instance Subst9 (a,b,c,d,e,f,g,h,i) i' = (a,b,c,d,e,f,g,h,i')

-- | Provides access to the 9th field of a tuple
class (Subst9 a (Field9 b) ~ b, Subst9 b (Field9 a) ~ a, Tuple8 a a, Tuple8 b b) => Tuple9 a b where
  -- | Access the 9th field of a tuple
  _9 :: Lens a b (Field9 a) (Field9 b)

instance Tuple9 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f,g,h,i') where
  _9 k (a,b,c,d,e,f,g,h,i) = (\i' -> (a,b,c,d,e,f,g,h,i')) <$> k i
  {-# INLINE _9 #-}

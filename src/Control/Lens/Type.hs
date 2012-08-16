{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

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
-- it wasn't a valid 'Getter' as Applicative isn't a superclass of
-- 'Gettable'.
--
-- 'Functor', however is the superclass of both.
--
-- @type 'Lens' a b c d = forall f. 'Functor' f => (c -> f d) -> a -> f b@
--
-- Every 'Lens' is a valid 'Setter', choosing @f@ =
-- 'Control.Lens.Getter.Mutator'.
--
-- Every 'Lens' can be used for 'Control.Lens.Getter.Getting' like a
-- 'Control.Lens.Fold.Fold' that doesn't use the 'Applicative' or
-- 'Control.Lens.Getter.Gettable'.
--
-- Every 'Lens' is a valid 'Control.Lens.Traversal.Traversal' that only uses
-- the 'Functor' part of the 'Applicative' it is supplied.
--
-- Every 'Lens' can be used for 'Control.Lens.Getter.Getting' like a valid
-- 'Getter', since 'Functor' is a superclass of 'Control.Lens.Getter.Gettable'
--
-- Since every 'Lens' can be used for 'Control.Lens.Getter.Getting' like a
-- valid 'Getter' it follows that it must view exactly one element in the
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

  -- * Traversing and Lensing
  , Zoom(..)

  -- * Common Lenses
  -- ** Tuples
  , Field1(..)
  , Field2(..)
  , Field3(..)
  , Field4(..)
  , Field5(..)
  , Field6(..)
  , Field7(..)
  , Field8(..)
  , Field9(..)
  -- ** Functions
  , resultAt

  -- * Lateral Composition
  , merged
  , alongside

  -- * Setting Functionally with Passthrough
  , (<%~), (<+~), (<-~), (<*~), (<//~)
  , (<^~), (<^^~), (<**~)
  , (<||~), (<&&~)

  -- * Setting State with Passthrough
  , (<%=), (<+=), (<-=), (<*=), (<//=)
  , (<^=), (<^^=), (<**=)
  , (<||=), (<&&=)

  -- * Cloning Lenses
  , cloneLens

  -- * Simplified and In-Progress
  , LensLike
  , Overloaded
  , SimpleLens
  , SimpleLensLike
  , SimpleOverloaded
  ) where

import Control.Applicative              as Applicative
import Control.Lens.Internal
import Control.Monad
import Control.Monad.State.Class        as State
import Control.Monad.Trans.State.Lazy   as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy   as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.RWS.Lazy   as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Data.Monoid

infixr 4 %%~
infix  4 %%=
infixr 4 <+~, <*~, <-~, <//~, <^~, <^^~, <**~, <&&~, <||~, <%~
infix  4 <+=, <*=, <-=, <//=, <^=, <^^=, <**=, <&&=, <||=, <%=


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
-- These laws are strong enough that the 4 type parameters of a 'Lens' cannot
-- vary fully independently. For more on how they interact, read the "Why is
-- it a Lens Family?" section of
-- <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- Every 'Lens' can be used directly as a 'Setter' or
-- 'Control.Lens.Traversal.Traversal'.
--
-- You can also use a 'Lens' for 'Control.Lens.Getter.Getting' as if it were a
-- 'Control.Lens.Fold.Fold' or 'Getter'.
--
-- Since every lens is a valid 'Control.Lens.Traversal.Traversal', the
-- traversal laws should also apply to any lenses you create.
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
-- @('%%~') = 'id'@
--
-- @
-- (%%~) :: 'Functor' f =>     'Control.Lens.Iso.Iso' a b c d       -> (c -> f d) -> a -> f b
-- (%%~) :: 'Functor' f =>     'Lens' a b c d      -> (c -> f d) -> a -> f b
-- (%%~) :: 'Applicative' f => 'Control.Lens.Traversal.Traversal' a b c d -> (c -> f d) -> a -> f b
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
-- (%%~) ::             'Control.Lens.Iso.Iso' a b c d       -> (c -> (e, d)) -> a -> (e, b)
-- (%%~) ::             'Lens' a b c d      -> (c -> (e, d)) -> a -> (e, b)
-- (%%~) :: 'Monoid' m => 'Control.Lens.Traversal.Traversal' a b c d -> (c -> (m, d)) -> a -> (m, b)
-- @
(%%~) :: LensLike f a b c d -> (c -> f d) -> a -> f b
(%%~) = id
{-# INLINE (%%~) #-}

-- | Modify the target of a 'Lens' in the current state returning some extra
-- information of @c@ or modify all targets of a
-- 'Control.Lens.Traversal.Traversal' in the current state, extracting extra
-- information of type @c@ and return a monoidal summary of the changes.
--
-- @('%%=') = ('state' '.')@
--
-- It may be useful to think of ('%%='), instead, as having either of the
-- following more restricted type signatures:
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

-- | This class allows us to use 'zoom' in, changing the State supplied by
-- many different monad transformers, potentially quite deep in a monad transformer stack.
class (MonadState s m, MonadState t n) => Zoom m n k s t | m -> s k, n -> t k, m t -> n, n s -> m where
  -- | Run a monadic action in a larger state than it was defined in,
  -- using a 'Simple' 'Lens' or 'Simple' 'Control.Lens.Traversal.Traversal'.
  --
  -- This is commonly used to lift actions in a simpler state monad into a
  -- state monad with a larger state type.
  --
  -- When applied to a 'Simple 'Control.Lens.Traversal.Traversal' over
  -- multiple values, the actions for each target are executed sequentially
  -- and the results are aggregated.
  --
  -- This can be used to edit pretty much any monad transformer stack with a state in it!
  --
  -- @
  -- zoom :: 'Monad' m               => 'Simple' 'Lens' a b      -> 'StateT' b m c -> 'StateT' a m c
  -- zoom :: ('Monad' m, 'Monoid' c) => 'Simple' 'Traversal' a b -> 'StateT' b m c -> 'StateT' a m c
  -- zoom :: 'Monad' m               => 'Simple' 'Lens' a b      -> 'RWST' r w b m c -> 'RWST' r w a m c
  -- zoom :: ('Monad' m, 'Monoid' c) => 'Simple' 'Traversal' a b -> 'RWST' r w b m c -> 'RWST' r w a m c
  -- zoom :: 'Monad' m               => 'Simple' 'Lens' a b      -> 'ErrorT' e ('RWST' r w b m c) -> 'ErrorT' e ('RWST' r w a m c)
  -- zoom :: ('Monad' m, 'Monoid' c) => 'Simple' 'Traversal' a b -> 'ErrorT' e ('RWST' r w b m c) -> 'ErrorT' e ('RWST' r w a m c)
  -- ...
  -- @
  zoom :: Monad m => SimpleLensLike (k c) t s -> m c -> n c

instance Monad z => Zoom (Strict.StateT s z) (Strict.StateT t z) (Focusing z) s t where
  zoom l (Strict.StateT m) = Strict.StateT $ unfocusing . l (Focusing . m)
  {-# INLINE zoom #-}

instance Monad z => Zoom (Lazy.StateT s z) (Lazy.StateT t z) (Focusing z) s t where
  zoom l (Lazy.StateT m) = Lazy.StateT $ unfocusing . l (Focusing . m)
  {-# INLINE zoom #-}

instance Zoom m n k s t => Zoom (ReaderT e m) (ReaderT e n) k s t where
  zoom l (ReaderT m) = ReaderT (zoom l . m)
  {-# INLINE zoom #-}

instance Zoom m n k s t => Zoom (IdentityT m) (IdentityT n) k s t where
  zoom l (IdentityT m) = IdentityT (zoom l m)
  {-# INLINE zoom #-}

instance (Monoid w, Monad z) => Zoom (Strict.RWST r w s z) (Strict.RWST r w t z) (FocusingWith w z) s t where
  zoom l (Strict.RWST m) = Strict.RWST $ \r -> unfocusingWith . l (FocusingWith . m r)
  {-# INLINE zoom #-}

instance (Monoid w, Monad z) => Zoom (Lazy.RWST r w s z) (Lazy.RWST r w t z) (FocusingWith w z) s t where
  zoom l (Lazy.RWST m) = Lazy.RWST $ \r -> unfocusingWith . l (FocusingWith . m r)
  {-# INLINE zoom #-}

instance (Monoid w, Zoom m n k s t) => Zoom (Strict.WriterT w m) (Strict.WriterT w n) (FocusingPlus w k) s t where
  zoom l = Strict.WriterT . zoom (\cfd -> unfocusingPlus . l (FocusingPlus  . cfd)) . Strict.runWriterT
  {-# INLINE zoom #-}

instance (Monoid w, Zoom m n k s t) => Zoom (Lazy.WriterT w m) (Lazy.WriterT w n) (FocusingPlus w k) s t where
  zoom l = Lazy.WriterT . zoom (\cfd -> unfocusingPlus . l (FocusingPlus  . cfd)) . Lazy.runWriterT
  {-# INLINE zoom #-}

instance Zoom m n k s t => Zoom (ListT m) (ListT n) (FocusingOn [] k) s t where
  zoom l = ListT . zoom (\cfd -> unfocusingOn . l (FocusingOn . cfd)) . runListT
  {-# INLINE zoom #-}

instance Zoom m n k s t => Zoom (MaybeT m) (MaybeT n) (FocusingOn Maybe k) s t where
  zoom l = MaybeT . zoom (\cfd -> unfocusingOn . l (FocusingOn . cfd)) . runMaybeT
  {-# INLINE zoom #-}

instance (Error e, Zoom m n k s t) => Zoom (ErrorT e m) (ErrorT e n) (FocusingErr e k) s t where
  zoom l = ErrorT . liftM getErr . zoom (\cfd -> unfocusingErr . l (FocusingErr . cfd)) . liftM Err . runErrorT
  {-# INLINE zoom #-}

-- TODO: instance Zoom m m k a a => Zoom (ContT r m) (ContT r m) k a a where

-------------------------------------------------------------------------------
-- Common Lenses
-------------------------------------------------------------------------------

-- | This lens can be used to change the result of a function but only where
-- the arguments match the key given.
resultAt :: Eq e => e -> Simple Lens (e -> a) a
resultAt e afa ea = go <$> afa a where
  a = ea e
  go a' e' | e == e'   = a'
           | otherwise = a
{-# INLINE resultAt #-}

-- | Merge two lenses, getters, setters, folds or traversals.
merged :: Functor f
       => LensLike f a b c c
       -> LensLike f a' b' c c
       -> LensLike f (Either a a') (Either b b') c c
merged l _ f (Left a)   = Left <$> l f a
merged _ r f (Right a') = Right <$> r f a'
{-# INLINE merged #-}

-- | 'alongside' makes a 'Lens' from two other lenses (or isomorphisms)
alongside :: Lens a b c d
           -> Lens a' b' c' d'
           -> Lens (a,a') (b,b') (c,c') (d,d')
alongside l r f (a, a') = case l (IndexedStore id) a of
  IndexedStore db c -> case r (IndexedStore id) a' of
    IndexedStore db' c' -> (\(d,d') -> (db d, db' d')) <$> f (c,c')
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
-- \"Costate Comonad Coalgebra is equivalent of Java's member variable
-- update technology for Haskell\" -- \@PLT_Borat on Twitter
cloneLens :: Functor f
      => LensLike (IndexedStore c d) a b c d
      -> (c -> f d) -> a -> f b
cloneLens f cfd a = case f (IndexedStore id) a of
  IndexedStore db c -> db <$> cfd c
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
-- When you do not need the result of the multiplication, ('*~') is more
-- flexible.
(<*~) :: Num c => LensLike ((,)c) a b c c -> c -> a -> (c, b)
l <*~ c = l <%~ (* c)
{-# INLINE (<*~) #-}

-- | Divide the target of a fractionally valued 'Lens' and return the result.
--
-- When you do not need the result of the division, ('//~') is more flexible.
(<//~) :: Fractional c => LensLike ((,)c) a b c c -> c -> a -> (c, b)
l <//~ c = l <%~ (/ c)
{-# INLINE (<//~) #-}

-- | Raise the target of a numerically valued 'Lens' to a non-negative
-- 'Integral' power and return the result
--
-- When you do not need the result of the division, ('^~') is more flexible.
(<^~) :: (Num c, Integral d) => LensLike ((,)c) a b c c -> d -> a -> (c, b)
l <^~ d = l <%~ (^ d)
{-# INLINE (<^~) #-}

-- | Raise the target of a fractionally valued 'Lens' to an 'Integral' power
-- and return the result.
--
-- When you do not need the result of the division, ('^^~') is more flexible.
(<^^~) :: (Fractional c, Integral d) => LensLike ((,)c) a b c c -> d -> a -> (c, b)
l <^^~ d = l <%~ (^^ d)
{-# INLINE (<^^~) #-}

-- | Raise the target of a floating-point valued 'Lens' to an arbitrary power
-- and return the result.
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

-------------------------------------------------------------------------------
-- Setting and Remembering State
-------------------------------------------------------------------------------

-- | Modify the target of a 'Lens' into your monad's state by a user supplied
-- function and return the result.
--
-- When you do not need the result of the operation, ('%=') is more flexible.
(<%=) :: MonadState a m => LensLike ((,)d) a a c d -> (c -> d) -> m d
l <%= f = l %%= (\c -> let d = f c in (d,d))
{-# INLINE (<%=) #-}

-- | Add to the target of a numerically valued 'Lens' into your monad's state
-- and return the result.
--
-- When you do not need the result of the multiplication, ('+=') is more
-- flexible.
(<+=) :: (MonadState a m, Num b) => SimpleLensLike ((,)b) a b -> b -> m b
l <+= b = l <%= (+ b)
{-# INLINE (<+=) #-}

-- | Subtract from the target of a numerically valued 'Lens' into your monad's
-- state and return the result.
--
-- When you do not need the result of the multiplication, ('-=') is more
-- flexible.
(<-=) :: (MonadState a m, Num b) => SimpleLensLike ((,)b) a b -> b -> m b
l <-= b = l <%= subtract b
{-# INLINE (<-=) #-}

-- | Multiply the target of a numerically valued 'Lens' into your monad's
-- state and return the result.
--
-- When you do not need the result of the multiplication, ('*=') is more
-- flexible.
(<*=) :: (MonadState a m, Num b) => SimpleLensLike ((,)b) a b -> b -> m b
l <*= b = l <%= (* b)
{-# INLINE (<*=) #-}

-- | Divide the target of a fractionally valued 'Lens' into your monad's state
-- and return the result.
--
-- When you do not need the result of the division, ('//=') is more flexible.
(<//=) :: (MonadState a m, Fractional b) => SimpleLensLike ((,)b) a b -> b -> m b
l <//= b = l <%= (/ b)
{-# INLINE (<//=) #-}

-- | Raise the target of a numerically valued 'Lens' into your monad's state
-- to a non-negative 'Integral' power and return the result.
--
-- When you do not need the result of the operation, ('**=') is more flexible.
(<^=) :: (MonadState a m, Num b, Integral c) => SimpleLensLike ((,)b) a b -> c -> m b
l <^= c = l <%= (^ c)
{-# INLINE (<^=) #-}

-- | Raise the target of a fractionally valued 'Lens' into your monad's state
-- to an 'Integral' power and return the result.
--
-- When you do not need the result of the operation, ('^^=') is more flexible.
(<^^=) :: (MonadState a m, Fractional b, Integral c) => SimpleLensLike ((,)b) a b -> c -> m b
l <^^= c = l <%= (^^ c)
{-# INLINE (<^^=) #-}

-- | Raise the target of a floating-point valued 'Lens' into your monad's
-- state to an arbitrary power and return the result.
--
-- When you do not need the result of the operation, ('**=') is more flexible.
(<**=) :: (MonadState a m, Floating b) => SimpleLensLike ((,)b) a b -> b -> m b
l <**= b = l <%= (** b)
{-# INLINE (<**=) #-}

-- | Logically '||' a Boolean valued 'Lens' into your monad's state and return
-- the result.
--
-- When you do not need the result of the operation, ('||=') is more flexible.
(<||=) :: MonadState a m => SimpleLensLike ((,)Bool) a Bool -> Bool -> m Bool
l <||= b = l <%= (|| b)
{-# INLINE (<||=) #-}

-- | Logically '&&' a Boolean valued 'Lens' into your monad's state and return
-- the result.
--
-- When you do not need the result of the operation, ('&&=') is more flexible.
(<&&=) :: MonadState a m => SimpleLensLike ((,)Bool) a Bool -> Bool -> m Bool
l <&&= b = l <%= (&& b)
{-# INLINE (<&&=) #-}

-- | Provides access to 1st field of a tuple.
class Field1 a b c d | a -> c, b -> d, a d -> b, b c -> a where
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
  _1 :: Lens a b c d

instance Field1 (a,b) (a',b) a a' where
  _1 k (a,b) = (\a' -> (a',b)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c) (a',b,c) a a' where
  _1 k (a,b,c) = (\a' -> (a',b,c)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d) (a',b,c,d) a a' where
  _1 k (a,b,c,d) = (\a' -> (a',b,c,d)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e) (a',b,c,d,e) a a' where
  _1 k (a,b,c,d,e) = (\a' -> (a',b,c,d,e)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f) (a',b,c,d,e,f) a a' where
  _1 k (a,b,c,d,e,f) = (\a' -> (a',b,c,d,e,f)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g) (a',b,c,d,e,f,g) a a' where
  _1 k (a,b,c,d,e,f,g) = (\a' -> (a',b,c,d,e,f,g)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g,h) (a',b,c,d,e,f,g,h) a a' where
  _1 k (a,b,c,d,e,f,g,h) = (\a' -> (a',b,c,d,e,f,g,h)) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f,g,h,i) (a',b,c,d,e,f,g,h,i) a a' where
  _1 k (a,b,c,d,e,f,g,h,i) = (\a' -> (a',b,c,d,e,f,g,h,i)) <$> k a
  {-# INLINE _1 #-}

-- | Provides access to the 2nd field of a tuple
class Field2 a b c d | a -> c, b -> d, a d -> b, b c -> a where
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
  _2 :: Lens a b c d

instance Field2 (a,b) (a,b') b b' where
  _2 k (a,b) = (\b' -> (a,b')) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c) (a,b',c) b b' where
  _2 k (a,b,c) = (\b' -> (a,b',c)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d) (a,b',c,d) b b' where
  _2 k (a,b,c,d) = (\b' -> (a,b',c,d)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e) (a,b',c,d,e) b b' where
  _2 k (a,b,c,d,e) = (\b' -> (a,b',c,d,e)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f) (a,b',c,d,e,f) b b' where
  _2 k (a,b,c,d,e,f) = (\b' -> (a,b',c,d,e,f)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g) (a,b',c,d,e,f,g) b b' where
  _2 k (a,b,c,d,e,f,g) = (\b' -> (a,b',c,d,e,f,g)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g,h) (a,b',c,d,e,f,g,h) b b' where
  _2 k (a,b,c,d,e,f,g,h) = (\b' -> (a,b',c,d,e,f,g,h)) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f,g,h,i) (a,b',c,d,e,f,g,h,i) b b' where
  _2 k (a,b,c,d,e,f,g,h,i) = (\b' -> (a,b',c,d,e,f,g,h,i)) <$> k b
  {-# INLINE _2 #-}

-- | Provides access to the 3rd field of a tuple
class Field3 a b c d | a -> c, b -> d, a d -> b, b c -> a where
  -- | Access the 3rd field of a tuple
  _3 :: Lens a b c d

instance Field3 (a,b,c) (a,b,c') c c' where
  _3 k (a,b,c) = (\c' -> (a,b,c')) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d) (a,b,c',d) c c' where
  _3 k (a,b,c,d) = (\c' -> (a,b,c',d)) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e) (a,b,c',d,e) c c' where
  _3 k (a,b,c,d,e) = (\c' -> (a,b,c',d,e)) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f) (a,b,c',d,e,f) c c' where
  _3 k (a,b,c,d,e,f) = (\c' -> (a,b,c',d,e,f)) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g) (a,b,c',d,e,f,g) c c' where
  _3 k (a,b,c,d,e,f,g) = (\c' -> (a,b,c',d,e,f,g)) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g,h) (a,b,c',d,e,f,g,h) c c' where
  _3 k (a,b,c,d,e,f,g,h) = (\c' -> (a,b,c',d,e,f,g,h)) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e,f,g,h,i) (a,b,c',d,e,f,g,h,i) c c' where
  _3 k (a,b,c,d,e,f,g,h,i) = (\c' -> (a,b,c',d,e,f,g,h,i)) <$> k c
  {-# INLINE _3 #-}

-- | Provide access to the 4th field of a tuple
class Field4 a b c d | a -> c, b -> d, a d -> b, b c -> a where
  -- | Access the 4th field of a tuple
  _4 :: Lens a b c d

instance Field4 (a,b,c,d) (a,b,c,d') d d' where
  _4 k (a,b,c,d) = (\d' -> (a,b,c,d')) <$> k d
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e) (a,b,c,d',e) d d' where
  _4 k (a,b,c,d,e) = (\d' -> (a,b,c,d',e)) <$> k d
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f) (a,b,c,d',e,f) d d' where
  _4 k (a,b,c,d,e,f) = (\d' -> (a,b,c,d',e,f)) <$> k d
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g) (a,b,c,d',e,f,g) d d' where
  _4 k (a,b,c,d,e,f,g) = (\d' -> (a,b,c,d',e,f,g)) <$> k d
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g,h) (a,b,c,d',e,f,g,h) d d' where
  _4 k (a,b,c,d,e,f,g,h) = (\d' -> (a,b,c,d',e,f,g,h)) <$> k d
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e,f,g,h,i) (a,b,c,d',e,f,g,h,i) d d' where
  _4 k (a,b,c,d,e,f,g,h,i) = (\d' -> (a,b,c,d',e,f,g,h,i)) <$> k d
  {-# INLINE _4 #-}

-- | Provides access to the 5th field of a tuple
class Field5 a b c d | a -> c, b -> d, a d -> b, b c -> a where
  -- | Access the 5th field of a tuple
  _5 :: Lens a b c d

instance Field5 (a,b,c,d,e) (a,b,c,d,e') e e' where
  _5 k (a,b,c,d,e) = (\e' -> (a,b,c,d,e')) <$> k e
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f) (a,b,c,d,e',f) e e' where
  _5 k (a,b,c,d,e,f) = (\e' -> (a,b,c,d,e',f)) <$> k e
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g) (a,b,c,d,e',f,g) e e' where
  _5 k (a,b,c,d,e,f,g) = (\e' -> (a,b,c,d,e',f,g)) <$> k e
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g,h) (a,b,c,d,e',f,g,h) e e' where
  _5 k (a,b,c,d,e,f,g,h) = (\e' -> (a,b,c,d,e',f,g,h)) <$> k e
  {-# INLINE _5 #-}

instance Field5 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e',f,g,h,i) e e' where
  _5 k (a,b,c,d,e,f,g,h,i) = (\e' -> (a,b,c,d,e',f,g,h,i)) <$> k e
  {-# INLINE _5 #-}

-- | Provides access to the 6th element of a tuple
class Field6 a b c d | a -> c, b -> d, a d -> b, b c -> a where
  -- | Access the 6th field of a tuple
  _6 :: Lens a b c d

instance Field6 (a,b,c,d,e,f) (a,b,c,d,e,f') f f' where
  _6 k (a,b,c,d,e,f) = (\f' -> (a,b,c,d,e,f')) <$> k f
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g) (a,b,c,d,e,f',g) f f' where
  _6 k (a,b,c,d,e,f,g) = (\f' -> (a,b,c,d,e,f',g)) <$> k f
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g,h) (a,b,c,d,e,f',g,h) f f' where
  _6 k (a,b,c,d,e,f,g,h) = (\f' -> (a,b,c,d,e,f',g,h)) <$> k f
  {-# INLINE _6 #-}

instance Field6 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f',g,h,i) f f' where
  _6 k (a,b,c,d,e,f,g,h,i) = (\f' -> (a,b,c,d,e,f',g,h,i)) <$> k f
  {-# INLINE _6 #-}

-- | Provide access to the 7th field of a tuple
class Field7 a b c d | a -> c, b -> d, a d -> b, b c -> a where
  -- | Access the 7th field of a tuple
  _7 :: Lens a b c d

instance Field7 (a,b,c,d,e,f,g) (a,b,c,d,e,f,g') g g' where
  _7 k (a,b,c,d,e,f,g) = (\g' -> (a,b,c,d,e,f,g')) <$> k g
  {-# INLINE _7 #-}

instance Field7 (a,b,c,d,e,f,g,h) (a,b,c,d,e,f,g',h) g g' where
  _7 k (a,b,c,d,e,f,g,h) = (\g' -> (a,b,c,d,e,f,g',h)) <$> k g
  {-# INLINE _7 #-}

instance Field7 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f,g',h,i) g g' where
  _7 k (a,b,c,d,e,f,g,h,i) = (\g' -> (a,b,c,d,e,f,g',h,i)) <$> k g
  {-# INLINE _7 #-}

-- | Provide access to the 8th field of a tuple
class Field8 a b c d | a -> c, b -> d, a d -> b, b c -> a where
  -- | Access the 8th field of a tuple
  _8 :: Lens a b c d

instance Field8 (a,b,c,d,e,f,g,h) (a,b,c,d,e,f,g,h') h h' where
  _8 k (a,b,c,d,e,f,g,h) = (\h' -> (a,b,c,d,e,f,g,h')) <$> k h
  {-# INLINE _8 #-}

instance Field8 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f,g,h',i) h h' where
  _8 k (a,b,c,d,e,f,g,h,i) = (\h' -> (a,b,c,d,e,f,g,h',i)) <$> k h
  {-# INLINE _8 #-}

-- | Provides access to the 9th field of a tuple
class Field9 a b c d | a -> c, b -> d, a d -> b, b c -> a where
  -- | Access the 9th field of a tuple
  _9 :: Lens a b c d

instance Field9 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f,g,h,i') i i' where
  _9 k (a,b,c,d,e,f,g,h,i) = (\i' -> (a,b,c,d,e,f,g,h,i')) <$> k i
  {-# INLINE _9 #-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Setter
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- A @'Setter' a b c d@ is a generalization of 'fmap' from 'Functor'. It allows you to map into a
--  structure and change out the contents, but it isn't strong enough to allow you to
--  enumerate those contents. Starting with @fmap :: 'Functor' f => (c -> d) -> f c -> f d@
--  we monomorphize the type to obtain @(c -> d) -> a -> b@ and then decorate it with 'Identity' to obtain
--
-- > type Setter a b c d = (c -> Identity d) -> a -> Identity b
--
--  Every 'Control.Lens.Traversal.Traversal' is a valid 'Setter', since 'Identity' is 'Applicative'.
--
-- Everything you can do with a 'Functor', you can do with a 'Setter'. There
-- are combinators that generalize 'fmap' and ('<$').
----------------------------------------------------------------------------
module Control.Lens.Setter
  (
  -- * Setters
    Setter
  , Settable(..)
  -- * Consuming Setters
  , Setting
  , Mutator(..)
  -- * Building Setters
  , sets
  -- * Common Setters
  , mapped
  -- * Functional Combinators
  , adjust
  , mapOf
  , set
  , (.~), (%~)
  , (+~), (-~), (*~), (//~), (^~), (^^~), (**~), (||~), (&&~), (<>~), (<.~)
  -- * State Combinators
  , (.=), (%=)
  , (+=), (-=), (*=), (//=), (^=), (^^=), (**=), (||=), (&&=), (<>=), (<.=)
  , (<~)
  -- * MonadWriter
  , whisper
  -- * Simplicity
  , SimpleSetter
  , SimpleSetting
  ) where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Monad.State.Class        as State
import Control.Monad.Writer.Class       as Writer
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Monoid

infixr 4 .~, +~, *~, -~, //~, ^~, ^^~, **~, &&~, ||~, %~, <>~, <.~
infix  4 .=, +=, *=, -=, //=, ^=, ^^=, **=, &&=, ||=, %=, <>=, <.=
infixr 2 <~


------------------------------------------------------------------------------
-- Setters
------------------------------------------------------------------------------

-- |
-- The only 'Control.Lens.Type.Lens'-like law that can apply to a 'Setter' @l@ is that
--
-- > set l c (set l b a) = set l c a
--
-- You can't 'view' a 'Setter' in general, so the other two laws are irrelevant.
--
-- However, two functor laws apply to a 'Setter'
--
-- > adjust l id = id
-- > adjust l f . adjust l g = adjust l (f . g)
--
-- These an be stated more directly:
--
-- > l pure = pure
-- > l f . run . l g = l (f . run . g)
--
-- You can compose a 'Setter' with a 'Control.Lens.Type.Lens' or a 'Control.Lens.Traversal.Traversal' using @(.)@ from the Prelude
-- and the result is always only a 'Setter' and nothing more.
type Setter a b c d = forall f. Settable f => (c -> f d) -> a -> f b

-- |
-- Running a Setter instantiates it to a concrete type.
--
-- When consuming a setter, use this type.
type Setting a b c d = (c -> Mutator d) -> a -> Mutator b

-- |
-- > 'SimpleSetter' = 'Simple' 'Setter'
type SimpleSetter a b = Setter a a b b

-- |
-- > 'SimpleSetting' m = 'Simple' 'Setting'
type SimpleSetting a b = Setting a a b b

-----------------------------------------------------------------------------
-- Settables & Mutators
-----------------------------------------------------------------------------

-- | Anything Settable must be isomorphic to the Identity Functor.
class Applicative f => Settable f where
  run :: f a -> a

instance Settable Identity where
  run = runIdentity

instance Settable f => Settable (Backwards f) where
  run = run . forwards

instance (Settable f, Settable g) => Settable (Compose f g) where
  run = run . run . getCompose

-- | 'Mutator' is just a renamed 'Identity' functor to give better error
-- messages when someone attempts to use a getter as a setter.
newtype Mutator a = Mutator { runMutator :: a }

instance Functor Mutator where
  fmap f (Mutator a) = Mutator (f a)

instance Applicative Mutator where
  pure = Mutator
  Mutator f <*> Mutator a = Mutator (f a)

instance Settable Mutator where
  run = runMutator

-----------------------------------------------------------------------------
-- Setters
-----------------------------------------------------------------------------

-- | This setter can be used to map over all of the values in a 'Functor'.
--
-- > fmap        = adjust mapped
-- > fmapDefault = adjust traverse
-- > (<$)        = set mapped
mapped :: Functor f => Setter (f a) (f b) a b
mapped = sets fmap
{-# INLINE mapped #-}

-- | Build a Setter from a map-like function.
--
-- Your supplied function @f@ is required to satisfy:
--
-- > f id = id
-- > f g . f h = f (g . h)
--
-- Equational reasoning:
--
-- > sets . adjust = id
-- > adjust . sets = id
--
-- Another way to view 'sets' is that it takes a 'semantic editor combinator'
-- and transforms it into a 'Setter'.
sets :: ((c -> d) -> a -> b) -> Setter a b c d
sets f g = pure . f (run . g)
{-# INLINE sets #-}

-----------------------------------------------------------------------------
-- Using Setters
-----------------------------------------------------------------------------

-- | Modify the target of a 'Control.Lens.Type.Lens' or all the targets of a 'Setter' or 'Control.Lens.Traversal.Traversal'
-- with a function.
--
-- > fmap        = adjust mapped
-- > fmapDefault = adjust traverse
--
-- > sets . adjust = id
-- > adjust . sets = id
--
-- > adjust :: Setter a b c d -> (c -> d) -> a -> b
--
-- Another way to view 'adjust' is to say that it transformers a 'Setter' into a
-- \"semantic editor combinator\".
adjust :: Setting a b c d -> (c -> d) -> a -> b
adjust l f = runMutator . l (Mutator . f)
{-# INLINE adjust #-}

-- | Modify the target of a 'Control.Lens.Type.Lens' or all the targets of a 'Setter' or 'Control.Lens.Traversal.Traversal'
-- with a function. This is an alias for adjust that is provided for consistency.
--
-- > mapOf = adjust
--
-- > fmap        = mapOf mapped
-- > fmapDefault = mapOf traverse
--
-- > sets . mapOf = id
-- > mapOf . sets = id
--
-- > mapOf :: Setter a b c d      -> (c -> d) -> a -> b
-- > mapOf :: Iso a b c d         -> (c -> d) -> a -> b
-- > mapOf :: Lens a b c d        -> (c -> d) -> a -> b
-- > mapOf :: Traversal a b c d   -> (c -> d) -> a -> b
mapOf :: Setting a b c d -> (c -> d) -> a -> b
mapOf = adjust
{-# INLINE mapOf #-}

-- | Replace the target of a 'Control.Lens.Type.Lens' or all of the targets of a 'Setter'
-- or 'Control.Lens.Traversal.Traversal' with a constant value.
--
-- > (<$) = set mapped
--
-- > set :: Setter a b c d    -> d -> a -> b
-- > set :: Iso a b c d       -> d -> a -> b
-- > set :: Lens a b c d      -> d -> a -> b
-- > set :: Traversal a b c d -> d -> a -> b
set :: Setting a b c d -> d -> a -> b
set l d = runMutator . l (\_ -> Mutator d)
{-# INLINE set #-}

-- | Modifies the target of a 'Control.Lens.Type.Lens' or all of the targets of a 'Setter' or
-- 'Control.Lens.Traversal.Traversal' with a user supplied function.
--
-- This is an infix version of 'adjust'
--
-- > fmap f = mapped %~ f
-- > fmapDefault f = traverse %~ f
--
-- > ghci> _2 %~ length $ (1,"hello")
-- > (1,5)
--
-- > (%~) :: Setter a b c d    -> (c -> d) -> a -> b
-- > (%~) :: Iso a b c d       -> (c -> d) -> a -> b
-- > (%~) :: Lens a b c d      -> (c -> d) -> a -> b
-- > (%~) :: Traversal a b c d -> (c -> d) -> a -> b
(%~) :: Setting a b c d -> (c -> d) -> a -> b
(%~) = adjust
{-# INLINE (%~) #-}

-- | Replace the target of a 'Control.Lens.Type.Lens' or all of the targets of a 'Setter'
-- or 'Control.Lens.Traversal.Traversal' with a constant value.
--
-- This is an infix version of 'set', provided for consistency with ('.=')
--
--
-- > f <$ a = mapped .~ f $ a
--
-- > ghci> bitAt 0 .~ True $ 0
-- > 1
--
-- > (.~) :: Setter a b c d    -> d -> a -> b
-- > (.~) :: Iso a b c d       -> d -> a -> b
-- > (.~) :: Lens a b c d      -> d -> a -> b
-- > (.~) :: Traversal a b c d -> d -> a -> b
(.~) :: Setting a b c d -> d -> a -> b
(.~) = set
{-# INLINE (.~) #-}

-- | Set with pass-through
--
-- This is mostly present for consistency, but may be useful for for chaining assignments
--
-- If you do not need a copy of the intermediate result, then using @l .~ d@ directly is a good idea.
(<.~) :: Setting a b c d -> d -> a -> (d, b)
l <.~ d = \a -> (d, l .~ d $ a)
{-# INLINE (<.~) #-}

-- | Increment the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal'
--
-- > ghci> _1 +~ 1 $ (1,2)
-- > (2,2)
(+~) :: Num c => Setting a b c c -> c -> a -> b
l +~ n = adjust l (+ n)
{-# INLINE (+~) #-}

-- | Multiply the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal'
--
-- >>> _2 *~ 4 $ (1,2)
-- (1,8)
(*~) :: Num c => Setting a b c c -> c -> a -> b
l *~ n = adjust l (* n)
{-# INLINE (*~) #-}

-- | Decrement the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal'
--
-- >>> _1 -~ 2 $ (1,2)
-- (-1,2)
(-~) :: Num c => Setting a b c c -> c -> a -> b
l -~ n = adjust l (subtract n)
{-# INLINE (-~) #-}

-- | Divide the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal'
(//~) :: Fractional c => Setting a b c c -> c -> a -> b
l //~ n = adjust l (/ n)

-- | Raise the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to a non-negative integral power
--
-- >>> _2 ^~ 2 $ (1,3)
-- (1,9)
(^~) :: (Num c, Integral e) => Setting a b c c -> e -> a -> b
l ^~ n = adjust l (^ n)
{-# INLINE (^~) #-}

-- | Raise the target(s) of a fractionally valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to an integral power
--
-- >>> _2 ^^~ (-1) $ (1,2)
-- (1,0.5)
(^^~) :: (Fractional c, Integral e) => Setting a b c c -> e -> a -> b
l ^^~ n = adjust l (^^ n)
{-# INLINE (^^~) #-}

-- | Raise the target(s) of a floating-point valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to an arbitrary power.
--
-- >>> _2 **~ pi $ (1,3)
-- (1,31.54428070019754)
(**~) :: Floating c => Setting a b c c -> c -> a -> b
l **~ n = adjust l (** n)
{-# INLINE (**~) #-}

-- | Logically '||' the target(s) of a 'Bool'-valued 'Control.Lens.Type.Lens' or 'Setter'
(||~):: Setting a b Bool Bool -> Bool -> a -> b
l ||~ n = adjust l (|| n)
{-# INLINE (||~) #-}

-- | Logically '&&' the target(s) of a 'Bool'-valued 'Control.Lens.Type.Lens' or 'Setter'
(&&~) :: Setting a b Bool Bool -> Bool -> a -> b
l &&~ n = adjust l (&& n)
{-# INLINE (&&~) #-}

-- | Modify the target of a monoidally valued by 'mappend'ing another value.
(<>~) :: Monoid c => Setting a b c c -> c -> a -> b
l <>~ n = adjust l (mappend n)
{-# INLINE (<>~) #-}

------------------------------------------------------------------------------
-- Using Setters with State
------------------------------------------------------------------------------

-- | Replace the target of a 'Control.Lens.Type.Lens' or all of the targets of a 'Setter' or 'Control.Lens.Traversal.Traversal' in our monadic
-- state with a new value, irrespective of the old.
--
-- > (.=) :: MonadState a m => Iso a a c d             -> d -> m ()
-- > (.=) :: MonadState a m => Lens a a c d            -> d -> m ()
-- > (.=) :: MonadState a m => Traversal a a c d       -> d -> m ()
-- > (.=) :: MonadState a m => Setter a a c d          -> d -> m ()
--
-- "It puts the state in the monad or it gets the hose again."
(.=) :: MonadState a m => Setting a a c d -> d -> m ()
l .= b = State.modify (l .~ b)
{-# INLINE (.=) #-}

-- | Map over the target of a 'Control.Lens.Type.Lens' or all of the targets of a 'Setter' or 'Traversal in our monadic state.
--
-- > (%=) :: MonadState a m => Iso a a c d             -> (c -> d) -> m ()
-- > (%=) :: MonadState a m => Lens a a c d            -> (c -> d) -> m ()
-- > (%=) :: MonadState a m => Traversal a a c d       -> (c -> d) -> m ()
-- > (%=) :: MonadState a m => Setter a a c d          -> (c -> d) -> m ()
(%=) :: MonadState a m => Setting a a c d -> (c -> d) -> m ()
l %= f = State.modify (l %~ f)
{-# INLINE (%=) #-}

-- | Modify the target(s) of a 'Simple' 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal' by adding a value
--
-- Example:
--
-- > fresh = do
-- >   id += 1
-- >   access id
(+=) :: (MonadState a m, Num b) => SimpleSetting a b -> b -> m ()
l += b = State.modify (l +~ b)
{-# INLINE (+=) #-}

-- | Modify the target(s) of a 'Simple' 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal' by subtracting a value
(-=) :: (MonadState a m, Num b) => SimpleSetting a b -> b -> m ()
l -= b = State.modify (l -~ b)
{-# INLINE (-=) #-}

-- | Modify the target(s) of a 'Simple' 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal' by multiplying by value
(*=) :: (MonadState a m, Num b) => SimpleSetting a b -> b -> m ()
l *= b = State.modify (l *~ b)
{-# INLINE (*=) #-}

-- | Modify the target(s) of a 'Simple' 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal' by dividing by a value
(//=) ::  (MonadState a m, Fractional b) => SimpleSetting a b -> b -> m ()
l //= b = State.modify (l //~ b)
{-# INLINE (//=) #-}

-- | Raise the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to a non-negative integral power
(^=) ::  (MonadState a m, Fractional b, Integral c) => SimpleSetting a b -> c -> m ()
l ^= c = State.modify (l ^~ c)
{-# INLINE (^=) #-}

-- | Raise the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to an integral power
(^^=) ::  (MonadState a m, Fractional b, Integral c) => SimpleSetting a b -> c -> m ()
l ^^= c = State.modify (l ^^~ c)
{-# INLINE (^^=) #-}

-- | Raise the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to an arbitrary power
(**=) ::  (MonadState a m, Floating b) => SimpleSetting a b -> b -> m ()
l **= b = State.modify (l **~ b)
{-# INLINE (**=) #-}

-- | Modify the target(s) of a 'Simple' 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal' by taking their logical '&&' with a value
(&&=):: MonadState a m => SimpleSetting a Bool -> Bool -> m ()
l &&= b = State.modify (l &&~ b)
{-# INLINE (&&=) #-}

-- | Modify the target(s) of a 'Simple' 'Control.Lens.Type.Lens', 'Iso, 'Setter' or 'Control.Lens.Traversal.Traversal' by taking their logical '||' with a value
(||=) :: MonadState a m => SimpleSetting a Bool -> Bool -> m ()
l ||= b = State.modify (l ||~ b)
{-# INLINE (||=) #-}

-- | Modify the target(s) of a 'Simple' 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal' by 'mappend'ing a value.
(<>=) :: (MonadState a m, Monoid b) => SimpleSetting a b -> b -> m ()
l <>= b = State.modify (l <>~ b)
{-# INLINE (<>=) #-}

-- | Run a monadic action, and set all of the targets of a 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to its result.
--
-- > (<~) :: MonadState a m => Iso a a c d       -> m d -> m ()
-- > (<~) :: MonadState a m => Lens a a c d      -> m d -> m ()
-- > (<~) :: MonadState a m => Traversal a a c d -> m d -> m ()
-- > (<~) :: MonadState a m => Setter a a c d    -> m d -> m ()
--
-- As a reasonable mnemonic, this lets you store the result of a monadic action in a lens rather than
-- in a local variable.
--
-- > do foo <- bar
-- >    ...
--
-- will store the result in a variable, while
--
-- > do foo <~ bar
-- >    ...
--
-- will store the result in a lens/setter/traversal.
(<~) :: MonadState a m => Setting a a c d -> m d -> m ()
l <~ md = md >>= (l .=)
{-# INLINE (<~) #-}

-- | Set with pass-through
--
-- This is useful for chaining assignment
--
-- > do x <- _2 <.= (an expensive expression)
--
-- If you do not need a copy of the intermediate result, then using @l .= d@ will avoid unused binding warnings
(<.=) :: MonadState a m => Setting a a c d -> d -> m d
l <.= d = do
  l .= d
  return d
{-# INLINE (<.=) #-}

------------------------------------------------------------------------------
-- MonadWriter
------------------------------------------------------------------------------

-- | Tell a part of a value to a 'MonadWriter', filling in the rest from 'mempty'
--
-- > whisper l d = tell (set l d mempty)

-- > whisper :: (MonadWriter b m, Monoid a) => Iso a b c d       -> d -> m ()
-- > whisper :: (MonadWriter b m, Monoid a) => Lens a b c d      -> d -> m ()
-- > whisper :: (MonadWriter b m, Monoid a) => Traversal a b c d -> d -> m ()
-- > whisper :: (MonadWriter b m, Monoid a) => Setter a b c d    -> d -> m ()
--
-- > whisper :: (MonadWriter b m, Monoid a) => ((c -> Identity d) -> a -> Identity b) -> d -> m ()
whisper :: (MonadWriter b m, Monoid a) => Setting a b c d -> d -> m ()
whisper l d = tell (set l d mempty)
{-# INLINE whisper #-}

-- Local definition for doctests to avoid cycles

_1 :: Functor f => (b -> f c) -> (b, a) -> f (c, a)
_1 f (a,b) = (\c -> (c,b)) <$> f a

_2 :: Functor f => (b -> f c) -> (a, b) -> f (a, c)
_2 f (a,b) = (,) a <$> f b

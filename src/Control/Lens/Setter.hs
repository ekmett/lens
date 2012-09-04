{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
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
-- structure and change out the contents, but it isn't strong enough to allow you to
-- enumerate those contents. Starting with @fmap :: 'Functor' f => (c -> d) -> f c -> f d@
-- we monomorphize the type to obtain @(c -> d) -> a -> b@ and then decorate it with 'Data.Functor.Identity.Identity' to obtain
--
-- @type 'Setter' a b c d = (c -> 'Data.Functor.Identity.Identity' d) -> a -> 'Data.Functor.Identity.Identity' b@
--
-- Every 'Control.Lens.Traversal.Traversal' is a valid 'Setter', since 'Data.Functor.Identity.Identity' is 'Applicative'.
--
-- Everything you can do with a 'Functor', you can do with a 'Setter'. There
-- are combinators that generalize 'fmap' and ('<$').
----------------------------------------------------------------------------
module Control.Lens.Setter
  (
  -- * Setters
    Setter
  -- * Building Setters
  , sets
  -- * Common Setters
  , mapped
  -- * Functional Combinators
  , over
  , mapOf
  , set
  , (.~), (%~)
  , (+~), (-~), (*~), (//~), (^~), (^^~), (**~), (||~), (&&~), (<.~)
  -- * State Combinators
  , assign
  , (.=), (%=)
  , (+=), (-=), (*=), (//=), (^=), (^^=), (**=), (||=), (&&=), (<.=)
  , (<~)
  -- * Storing Setters
  , ReifiedSetter(..)
  -- * Setter Internals
  , Setting
  , SimpleSetting
  -- * Simplicity
  , SimpleSetter
  , SimpleReifiedSetter
  ) where

import Control.Applicative
import Control.Lens.Internal
import Control.Monad.State.Class as State

infixr 4 .~, +~, *~, -~, //~, ^~, ^^~, **~, &&~, ||~, %~, <.~
infix  4 .=, +=, *=, -=, //=, ^=, ^^=, **=, &&=, ||=, %=, <.=
infixr 2 <~

------------------------------------------------------------------------------
-- Setters
------------------------------------------------------------------------------

-- |
-- The only 'Control.Lens.Type.Lens'-like law that can apply to a 'Setter' @l@ is that
--
-- @'set' l c ('set' l b a) ≡ 'set' l c a@
--
-- You can't 'view' a 'Setter' in general, so the other two laws are irrelevant.
--
-- However, two 'Functor' laws apply to a 'Setter':
--
-- @
-- 'over' l 'id' ≡ 'id'
-- 'over' l f '.' 'over' l g ≡ 'over' l (f '.' g)
-- @
--
-- These an be stated more directly:
--
-- @
-- l 'pure' ≡ 'pure'
-- l f . 'untainted' . l g ≡ l (f . 'untainted' . g)
-- @
--
-- You can compose a 'Setter' with a 'Control.Lens.Type.Lens' or a 'Control.Lens.Traversal.Traversal' using ('.') from the Prelude
-- and the result is always only a 'Setter' and nothing more.
type Setter a b c d = forall f. Settable f => (c -> f d) -> a -> f b

-- |
-- Running a 'Setter' instantiates it to a concrete type.
--
-- When consuming a setter directly to perform a mapping, you can use this type, but most
-- user code will not need to use this type.
--
-- By choosing 'Mutator' rather than 'Data.Functor.Identity.Identity', we get nicer error messages.
type Setting a b c d = (c -> Mutator d) -> a -> Mutator b

-- |
--
-- A Simple Setter is just a 'Setter' that doesn't change the types.
--
-- These are particularly common when talking about monomorphic containers. /e.g./
--
-- @'sets' Data.Text.map :: 'SimpleSetter' 'Data.Text.Internal.Text' 'Char'@
--
-- @type 'SimpleSetter' = 'Control.Lens.Type.Simple' 'Setter'@
type SimpleSetter a b = Setter a a b b

-- |
-- This is a useful alias for use when consuming a 'SimpleSetter'.
--
-- Most user code will never have to use this type.
--
-- @type 'SimpleSetting' m = 'Control.Lens.Type.Simple' 'Setting'@
type SimpleSetting a b = Setting a a b b

-----------------------------------------------------------------------------
-- Setters
-----------------------------------------------------------------------------

-- | This setter can be used to map over all of the values in a 'Functor'.
--
-- @
-- 'fmap' ≡ 'over' 'mapped'
-- 'Data.Traversable.fmapDefault' ≡ 'over' 'Data.Traversable.traverse'
-- ('<$') ≡ 'set' 'mapped'
-- @
--
-- >>> import Control.Lens
-- >>> over mapped (+1) [1,2,3]
-- [2,3,4]
--
-- >>> set mapped () [1,2,3]
-- [(),(),()]
--
-- >>> mapped.mapped %~ (+1) $ [[1,2],[3]]
-- [[2,3],[4]]
--
-- >>> over (mapped._2) length [("hello","world"),("leaders","!!!")]
-- [("hello",5),("leaders",3)]
mapped :: Functor f => Setter (f a) (f b) a b
mapped = sets fmap
{-# INLINE mapped #-}

-- | Build a Setter from a map-like function.
--
-- Your supplied function @f@ is required to satisfy:
--
-- @
-- f 'id' ≡ 'id'
-- f g '.' f h ≡ f (g '.' h)
-- @
--
-- Equational reasoning:
--
-- @
-- 'sets' '.' 'over' ≡ 'id'
-- 'over' '.' 'sets' ≡ 'id'
-- @
--
-- Another way to view 'sets' is that it takes a \"semantic editor combinator\"
-- and transforms it into a 'Setter'.
sets :: ((c -> d) -> a -> b) -> Setter a b c d
sets f g = pure . f (untainted . g)
{-# INLINE sets #-}

-----------------------------------------------------------------------------
-- Using Setters
-----------------------------------------------------------------------------

-- | Modify the target of a 'Control.Lens.Type.Lens' or all the targets of a 'Setter' or 'Control.Lens.Traversal.Traversal'
-- with a function.
--
-- @
-- 'fmap' ≡ 'over' 'mapped'
-- 'Data.Traversable.fmapDefault' ≡ 'over' 'Data.Traversable.traverse'
-- 'sets' '.' 'over' ≡ 'id'
-- 'over' '.' 'sets' ≡ 'id'
-- @
--
-- >>> import Control.Lens
-- >>> over mapped (*10) [1,2,3]
-- [10,20,30]
--
-- >>> over _1 show (10,20)
-- ("10",20)
--
-- Another way to view 'over' is to say that it transformers a 'Setter' into a
-- \"semantic editor combinator\".
--
-- @'over' :: 'Setter' a b c d -> (c -> d) -> a -> b@
over :: Setting a b c d -> (c -> d) -> a -> b
over l f = runMutator . l (Mutator . f)
{-# INLINE over #-}

-- | Modify the target of a 'Control.Lens.Type.Lens' or all the targets of a 'Setter' or 'Control.Lens.Traversal.Traversal'
-- with a function. This is an alias for 'over' that is provided for consistency.
--
-- @
-- 'mapOf' ≡ 'over'
-- 'fmap' ≡ 'mapOf' 'mapped'
-- 'fmapDefault' ≡ 'mapOf' 'traverse'
-- 'sets' '.' 'mapOf' ≡ 'id'
-- 'mapOf' '.' 'sets' ≡ 'id'
-- @
--
-- >>> import Control.Lens
-- >>> mapOf mapped (+1) [1,2,3,4]
-- [2,3,4,5]
--
-- >>> mapOf _1 (+1) (1,2)
-- (2,2)
--
-- >>> mapOf both (+1) (1,2)
-- (2,3)
--
-- @
-- 'mapOf' :: 'Setter' a b c d      -> (c -> d) -> a -> b
-- 'mapOf' :: 'Control.Lens.Iso.Iso' a b c d         -> (c -> d) -> a -> b
-- 'mapOf' :: 'Control.Lens.Type.Lens' a b c d        -> (c -> d) -> a -> b
-- 'mapOf' :: 'Control.Lens.Traversal.Traversal' a b c d   -> (c -> d) -> a -> b
-- @
mapOf :: Setting a b c d -> (c -> d) -> a -> b
mapOf = over
{-# INLINE mapOf #-}

-- | Replace the target of a 'Control.Lens.Type.Lens' or all of the targets of a 'Setter'
-- or 'Control.Lens.Traversal.Traversal' with a constant value.
--
-- @('<$') ≡ 'set' 'mapped'@
--
-- >>> import Control.Lens
-- >>> set _2 "hello" (1,())
-- (1,"hello")
--
-- >>> set mapped () [1,2,3,4]
-- [(),(),(),()]
--
-- Note: Attempting to 'set' a 'Fold' or 'Getter' will fail at compile time with an
-- relatively nice error message.
--
-- @
-- 'set' :: 'Setter' a b c d    -> d -> a -> b
-- 'set' :: 'Control.Lens.Iso.Iso' a b c d       -> d -> a -> b
-- 'set' :: 'Control.Lens.Type.Lens' a b c d      -> d -> a -> b
-- 'set' :: 'Control.Lens.Traversal.Traversal' a b c d -> d -> a -> b
-- @
set :: Setting a b c d -> d -> a -> b
set l d = runMutator . l (\_ -> Mutator d)
{-# INLINE set #-}

-- | Modifies the target of a 'Control.Lens.Type.Lens' or all of the targets of a 'Setter' or
-- 'Control.Lens.Traversal.Traversal' with a user supplied function.
--
-- This is an infix version of 'over'
--
-- @
-- 'fmap' f ≡ 'mapped' '%~' f
-- 'Data.Traversable.fmapDefault' f ≡ 'traverse' '%~' f
-- @
--
-- >>> import Control.Lens
-- >>> _2 %~ length $ (1,"hello")
-- (1,5)
--
-- >>> traverse %~ (+1) $ [1,2,3]
-- [2,3,4]
--
-- >>> _2 %~ (+1) $ (3,4)
-- (3,5)
--
-- >>> traverse.traverse %~ length $ [["hello","world"],["!!!"]]
-- [[5,5],[3]]
--
-- @
-- ('%~') :: 'Setter' a b c d    -> (c -> d) -> a -> b
-- ('%~') :: 'Control.Lens.Iso.Iso' a b c d       -> (c -> d) -> a -> b
-- ('%~') :: 'Control.Lens.Type.Lens' a b c d      -> (c -> d) -> a -> b
-- ('%~') :: 'Control.Lens.Traversal.Traversal' a b c d -> (c -> d) -> a -> b
-- @
(%~) :: Setting a b c d -> (c -> d) -> a -> b
(%~) = over
{-# INLINE (%~) #-}

-- | Replace the target of a 'Control.Lens.Type.Lens' or all of the targets of a 'Setter'
-- or 'Control.Lens.Traversal.Traversal' with a constant value.
--
-- This is an infix version of 'set', provided for consistency with ('.=')
--
-- @f '<$' a ≡ 'mapped' '.~' f '$' a@
--
-- >>> import Control.Lens
-- >>> _1 .~ "hello" $ (42,"world")
-- ("hello","world")
--
-- @
-- ('.~') :: 'Setter' a b c d    -> d -> a -> b
-- ('.~') :: 'Control.Lens.Iso.Iso' a b c d       -> d -> a -> b
-- ('.~') :: 'Control.Lens.Type.Lens' a b c d      -> d -> a -> b
-- ('.~') :: 'Control.Lens.Traversal.Traversal' a b c d -> d -> a -> b
-- @
(.~) :: Setting a b c d -> d -> a -> b
(.~) = set
{-# INLINE (.~) #-}

-- | Set with pass-through
--
-- This is mostly present for consistency, but may be useful for for chaining assignments
--
-- If you do not need a copy of the intermediate result, then using @l '.~' d@ directly is a good idea.
--
-- >>> import Control.Lens
-- >>> _3 <.~ "world" $ ("good","morning","vietnam")
-- ("world",("good","morning","world"))
--
-- >>> import Data.Map as Map
-- >>> _2.at "hello" <.~ Just "world" $ (42,Map.fromList [("goodnight","gracie")])
-- (Just "world",(42,fromList [("goodnight","gracie"),("hello","world")]))
--
-- @
-- ('<.~') :: 'Setter' a b c d    -> d -> a -> (d, b)
-- ('<.~') :: 'Control.Lens.Iso.Iso' a b c d       -> d -> a -> (d, b)
-- ('<.~') :: 'Control.Lens.Type.Lens' a b c d      -> d -> a -> (d, b)
-- ('<.~') :: 'Control.Lens.Traversal.Traversal' a b c d -> d -> a -> (d, b)
-- @
(<.~) :: Setting a b c d -> d -> a -> (d, b)
l <.~ d = \a -> (d, l .~ d $ a)
{-# INLINE (<.~) #-}

-- | Increment the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal'
--
-- >>> import Control.Lens
-- >>> _1 +~ 1 $ (1,2)
-- (2,2)
--
-- >>> both +~ 2 $ (5,6)
-- (7,8)
--
-- @
-- ('+~') :: Num b => 'Control.Lens.Type.Simple' 'Setter' a b -> b -> a -> a
-- ('+~') :: Num b => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a b -> b -> a -> a
-- ('+~') :: Num b => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a b -> b -> a -> a
-- ('+~') :: Num b => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a b -> b -> a -> a
-- @
(+~) :: Num c => Setting a b c c -> c -> a -> b
l +~ n = over l (+ n)
{-# INLINE (+~) #-}

-- | Multiply the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal'
--
-- >>> import Control.Lens
-- >>> _2 *~ 4 $ (1,2)
-- (1,8)
--
-- >>> mapped *~ 2 $ Just 24
-- Just 48
--
-- @
-- ('*~') :: 'Num' b => 'Control.Lens.Type.Simple' 'Setter' a b -> b -> a -> a
-- ('*~') :: 'Num' b => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a b -> b -> a -> a
-- ('*~') :: 'Num' b => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a b -> b -> a -> a
-- ('*~') :: 'Num' b => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a b -> b -> a -> a
-- @
(*~) :: Num c => Setting a b c c -> c -> a -> b
l *~ n = over l (* n)
{-# INLINE (*~) #-}

-- | Decrement the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal'
--
-- >>> import Control.Lens
-- >>> _1 -~ 2 $ (1,2)
-- (-1,2)
--
-- >>> mapped.mapped -~ 1 $ [[4,5],[6,7]]
-- [[3,4],[5,6]]
--
-- @
-- ('-~') :: 'Num' b => 'Control.Lens.Type.Simple' 'Setter' a b -> b -> a -> a
-- ('-~') :: 'Num' b => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a b -> b -> a -> a
-- ('-~') :: 'Num' b => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a b -> b -> a -> a
-- ('-~') :: 'Num' b => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a b -> b -> a -> a
-- @
(-~) :: Num c => Setting a b c c -> c -> a -> b
l -~ n = over l (subtract n)
{-# INLINE (-~) #-}

-- | Divide the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal'
--
-- >>> import Control.Lens
-- >>> _2 //~ 2 $ ("Hawaii",10)
-- ("Hawaii",5.0)
--
-- @
-- ('//~') :: 'Fractional' b => 'Control.Lens.Type.Simple' 'Setter' a b -> b -> a -> a
-- ('//~') :: 'Fractional' b => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a b -> b -> a -> a
-- ('//~') :: 'Fractional' b => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a b -> b -> a -> a
-- ('//~') :: 'Fractional' b => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a b -> b -> a -> a
-- @
(//~) :: Fractional c => Setting a b c c -> c -> a -> b
l //~ n = over l (/ n)

-- | Raise the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to a non-negative integral power
--
-- >>> import Control.Lens
-- >>> _2 ^~ 2 $ (1,3)
-- (1,9)
--
-- @
-- ('^~') :: ('Num' b, 'Integral' c) => 'Control.Lens.Type.Simple' 'Setter' a b -> c -> a -> a
-- ('^~') :: ('Num' b, 'Integral' c) => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a b -> c -> a -> a
-- ('^~') :: ('Num' b, 'Integral' c) => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a b -> c -> a -> a
-- ('^~') :: ('Num' b, 'Integral' c) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a b -> c -> a -> a
-- @
(^~) :: (Num c, Integral e) => Setting a b c c -> e -> a -> b
l ^~ n = over l (^ n)
{-# INLINE (^~) #-}

-- | Raise the target(s) of a fractionally valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to an integral power
--
-- >>> import Control.Lens
-- >>> _2 ^^~ (-1) $ (1,2)
-- (1,0.5)
--
-- @
-- ('^^~') :: ('Fractional' b, 'Integral' c) => 'Control.Lens.Type.Simple' 'Setter' a b -> c -> a -> a
-- ('^^~') :: ('Fractional' b, 'Integral' c) => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a b -> c -> a -> a
-- ('^^~') :: ('Fractional' b, 'Integral' c) => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a b -> c -> a -> a
-- ('^^~') :: ('Fractional' b, 'Integral' c) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a b -> c -> a -> a
-- @
--
(^^~) :: (Fractional c, Integral e) => Setting a b c c -> e -> a -> b
l ^^~ n = over l (^^ n)
{-# INLINE (^^~) #-}

-- | Raise the target(s) of a floating-point valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to an arbitrary power.
--
-- >>> import Control.Lens
-- >>> _2 **~ pi $ (1,3)
-- (1,31.54428070019754)
--
-- @
-- ('**~') :: 'Floating' b => 'Control.Lens.Type.Simple' 'Setter' a b -> b -> a -> a
-- ('**~') :: 'Floating' b => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a b -> b -> a -> a
-- ('**~') :: 'Floating' b => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a b -> b -> a -> a
-- ('**~') :: 'Floating' b => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a b -> b -> a -> a
-- @
(**~) :: Floating c => Setting a b c c -> c -> a -> b
l **~ n = over l (** n)
{-# INLINE (**~) #-}

-- | Logically '||' the target(s) of a 'Bool'-valued 'Control.Lens.Type.Lens' or 'Setter'
--
-- >>> import Control.Lens
-- >>> both ||~ True $ (False,True)
-- (True,True)
--
-- >>> both ||~ False $ (False,True)
-- (False,True)
--
-- @
-- ('||~') :: 'Control.Lens.Type.Simple' 'Setter' a 'Bool' -> 'Bool' -> a -> a
-- ('||~') :: 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a 'Bool' -> 'Bool' -> a -> a
-- ('||~') :: 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a 'Bool' -> 'Bool' -> a -> a
-- ('||~') :: 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a 'Bool' -> 'Bool' -> a -> a
-- @
(||~):: Setting a b Bool Bool -> Bool -> a -> b
l ||~ n = over l (|| n)
{-# INLINE (||~) #-}

-- | Logically '&&' the target(s) of a 'Bool'-valued 'Control.Lens.Type.Lens' or 'Setter'
--
-- >>> import Control.Lens
-- >>> both &&~ True $ (False, True)
-- (False,True)
--
-- >>> both &&~ False $ (False, True)
-- (False,False)
--
-- @
-- ('&&~') :: 'Control.Lens.Type.Simple' 'Setter' a 'Bool' -> 'Bool' -> a -> a
-- ('&&~') :: 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a 'Bool' -> 'Bool' -> a -> a
-- ('&&~') :: 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a 'Bool' -> 'Bool' -> a -> a
-- ('&&~') :: 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a 'Bool' -> 'Bool' -> a -> a
-- @
(&&~) :: Setting a b Bool Bool -> Bool -> a -> b
l &&~ n = over l (&& n)
{-# INLINE (&&~) #-}

------------------------------------------------------------------------------
-- Using Setters with State
------------------------------------------------------------------------------

-- | Replace the target of a 'Control.Lens.Type.Lens' or all of the targets of a 'Setter' or 'Control.Lens.Traversal.Traversal' in our monadic
-- state with a new value, irrespective of the old.
--
-- This is an alias for ('.=').
--
-- @
-- 'assign' :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a b       -> b -> m ()
-- 'assign' :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a b      -> b -> m ()
-- 'assign' :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a b -> b -> m ()
-- 'assign' :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Setter' a b    -> b -> m ()
-- @
assign :: MonadState a m => Setting a a c d -> d -> m ()
assign l b = State.modify (set l b)
{-# INLINE assign #-}

-- | Replace the target of a 'Control.Lens.Type.Lens' or all of the targets of a 'Setter' or 'Control.Lens.Traversal.Traversal' in our monadic
-- state with a new value, irrespective of the old.
--
-- This is an infix version of 'assign'.
--
-- @
-- ('.=') :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a b       -> b -> m ()
-- ('.=') :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a b      -> b -> m ()
-- ('.=') :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a b -> b -> m ()
-- ('.=') :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Setter' a b    -> b -> m ()
-- @
--
-- "It puts the state in the monad or it gets the hose again."
(.=) :: MonadState a m => Setting a a c d -> d -> m ()
l .= b = State.modify (l .~ b)
{-# INLINE (.=) #-}

-- | Map over the target of a 'Control.Lens.Type.Lens' or all of the targets of a 'Setter' or 'Control.Lens.Traversal.Traversal' in our monadic state.
--
-- @
-- ('%=') :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a b       -> (b -> b) -> m ()
-- ('%=') :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a b      -> (b -> b) -> m ()
-- ('%=') :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a b -> (b -> b) -> m ()
-- ('%=') :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Setter' a b    -> (b -> b) -> m ()
-- @
(%=) :: MonadState a m => Setting a a c d -> (c -> d) -> m ()
l %= f = State.modify (l %~ f)
{-# INLINE (%=) #-}

-- | Modify the target(s) of a 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal' by adding a value
--
-- Example:
--
-- @
-- fresh :: MonadState Int m => m Int
-- fresh = do
--   'id' '+=' 1
--   'use' 'id'
-- @
--
-- @
-- ('+=') :: ('MonadState' a m, 'Num' b) => 'Control.Lens.Type.Simple' 'Setter' a b -> b -> m ()
-- ('+=') :: ('MonadState' a m, 'Num' b) => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a b -> b -> m ()
-- ('+=') :: ('MonadState' a m, 'Num' b) => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a b -> b -> m ()
-- ('+=') :: ('MonadState' a m, 'Num' b) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a b -> b -> m ()
-- @
(+=) :: (MonadState a m, Num b) => SimpleSetting a b -> b -> m ()
l += b = State.modify (l +~ b)
{-# INLINE (+=) #-}

-- | Modify the target(s) of a 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal' by subtracting a value
--
-- @
-- ('-=') :: ('MonadState' a m, 'Num' b) => 'Control.Lens.Type.Simple' 'Setter' a b -> b -> m ()
-- ('-=') :: ('MonadState' a m, 'Num' b) => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a b -> b -> m ()
-- ('-=') :: ('MonadState' a m, 'Num' b) => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a b -> b -> m ()
-- ('-=') :: ('MonadState' a m, 'Num' b) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a b -> b -> m ()
-- @
(-=) :: (MonadState a m, Num b) => SimpleSetting a b -> b -> m ()
l -= b = State.modify (l -~ b)
{-# INLINE (-=) #-}

-- | Modify the target(s) of a 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal' by multiplying by value.
--
-- @
-- ('*=') :: ('MonadState' a m, 'Num' b) => 'Control.Lens.Type.Simple' 'Setter' a b -> b -> m ()
-- ('*=') :: ('MonadState' a m, 'Num' b) => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a b -> b -> m ()
-- ('*=') :: ('MonadState' a m, 'Num' b) => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a b -> b -> m ()
-- ('*=') :: ('MonadState' a m, 'Num' b) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a b -> b -> m ()
-- @
(*=) :: (MonadState a m, Num b) => SimpleSetting a b -> b -> m ()
l *= b = State.modify (l *~ b)
{-# INLINE (*=) #-}

-- | Modify the target(s) of a 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal' by dividing by a value.
--
-- @
-- ('//=') :: ('MonadState' a m, 'Fractional' b) => 'Control.Lens.Type.Simple' 'Setter' a b -> b -> m ()
-- ('//=') :: ('MonadState' a m, 'Fractional' b) => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a b -> b -> m ()
-- ('//=') :: ('MonadState' a m, 'Fractional' b) => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a b -> b -> m ()
-- ('//=') :: ('MonadState' a m, 'Fractional' b) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a b -> b -> m ()
-- @
(//=) :: (MonadState a m, Fractional b) => SimpleSetting a b -> b -> m ()
l //= b = State.modify (l //~ b)
{-# INLINE (//=) #-}

-- | Raise the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to a non-negative integral power.
--
-- @
-- ('^=') ::  ('MonadState' a m, 'Fractional' b, 'Integral' c) => 'Control.Lens.Type.Simple' 'Setter' a b -> c -> m ()
-- ('^=') ::  ('MonadState' a m, 'Fractional' b, 'Integral' c) => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a b -> c -> m ()
-- ('^=') ::  ('MonadState' a m, 'Fractional' b, 'Integral' c) => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a b -> c -> m ()
-- ('^=') ::  ('MonadState' a m, 'Fractional' b, 'Integral' c) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a b -> c -> m ()
-- @
(^=) :: (MonadState a m, Fractional b, Integral c) => SimpleSetting a b -> c -> m ()
l ^= c = State.modify (l ^~ c)
{-# INLINE (^=) #-}

-- | Raise the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to an integral power.
--
-- @
-- ('^^=') ::  ('MonadState' a m, 'Fractional' b, 'Integral' c) => 'Control.Lens.Type.Simple' 'Setter' a b -> c -> m ()
-- ('^^=') ::  ('MonadState' a m, 'Fractional' b, 'Integral' c) => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a b -> c -> m ()
-- ('^^=') ::  ('MonadState' a m, 'Fractional' b, 'Integral' c) => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a b -> c -> m ()
-- ('^^=') ::  ('MonadState' a m, 'Fractional' b, 'Integral' c) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a b -> c -> m ()
-- @
(^^=) :: (MonadState a m, Fractional b, Integral c) => SimpleSetting a b -> c -> m ()
l ^^= c = State.modify (l ^^~ c)
{-# INLINE (^^=) #-}

-- | Raise the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to an arbitrary power
--
-- @
-- ('**=') ::  ('MonadState' a m, 'Floating' b) => 'Control.Lens.Type.Simple' 'Setter' a b -> b -> m ()
-- ('**=') ::  ('MonadState' a m, 'Floating' b) => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a b -> b -> m ()
-- ('**=') ::  ('MonadState' a m, 'Floating' b) => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a b -> b -> m ()
-- ('**=') ::  ('MonadState' a m, 'Floating' b) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a b -> b -> m ()
-- @
(**=) :: (MonadState a m, Floating b) => SimpleSetting a b -> b -> m ()
l **= b = State.modify (l **~ b)
{-# INLINE (**=) #-}

-- | Modify the target(s) of a 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal' by taking their logical '&&' with a value
--
-- @
-- ('&&=') :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Setter' a 'Bool' -> 'Bool' -> m ()
-- ('&&=') :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a 'Bool' -> 'Bool' -> m ()
-- ('&&=') :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a 'Bool' -> 'Bool' -> m ()
-- ('&&=') :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a 'Bool' -> 'Bool' -> m ()
-- @
(&&=):: MonadState a m => SimpleSetting a Bool -> Bool -> m ()
l &&= b = State.modify (l &&~ b)
{-# INLINE (&&=) #-}

-- | Modify the target(s) of a 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens', 'Iso, 'Setter' or 'Control.Lens.Traversal.Traversal' by taking their logical '||' with a value
--
-- @
-- ('||=') :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Setter' a 'Bool' -> 'Bool' -> m ()
-- ('||=') :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' a 'Bool' -> 'Bool' -> m ()
-- ('||=') :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' a 'Bool' -> 'Bool' -> m ()
-- ('||=') :: 'MonadState' a m => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' a 'Bool' -> 'Bool' -> m ()
-- @
(||=) :: MonadState a m => SimpleSetting a Bool -> Bool -> m ()
l ||= b = State.modify (l ||~ b)
{-# INLINE (||=) #-}

-- | Run a monadic action, and set all of the targets of a 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to its result.
--
-- @
-- ('<~') :: 'MonadState' a m => 'Control.Lens.Iso.Iso' a a c d       -> m d -> m ()
-- ('<~') :: 'MonadState' a m => 'Control.Lens.Type.Lens' a a c d      -> m d -> m ()
-- ('<~') :: 'MonadState' a m => 'Control.Lens.Traversal.Traversal' a a c d -> m d -> m ()
-- ('<~') :: 'MonadState' a m => 'Setter' a a c d    -> m d -> m ()
-- @
--
-- As a reasonable mnemonic, this lets you store the result of a monadic action in a lens rather than
-- in a local variable.
--
-- @
-- do foo <- bar
--    ...
-- @
--
-- will store the result in a variable, while
--
-- @
-- do foo '<~' bar
--    ...
-- @
--
-- will store the result in a 'Control.Lens.Type.Lens', 'Setter', or 'Control.Lens.Traversal.Traversal'.
(<~) :: MonadState a m => Setting a a c d -> m d -> m ()
l <~ md = md >>= (l .=)
{-# INLINE (<~) #-}

-- | Set with pass-through
--
-- This is useful for chaining assignment without round-tripping through your monad stack.
--
-- @do x <- '_2' <.= ninety_nine_bottles_of_beer_on_the_wall@
--
-- If you do not need a copy of the intermediate result, then using @l .= d@ will avoid unused binding warnings
--
-- @
-- ('<.=') :: 'MonadState' a m => 'Setter' a a c d -> d -> m d
-- ('<.=') :: 'MonadState' a m => 'Control.Lens.Iso.Iso' a a c d -> d -> m d
-- ('<.=') :: 'MonadState' a m => 'Control.Lens.Type.Lens' a a c d -> d -> m d
-- ('<.=') :: 'MonadState' a m => 'Control.Lens.Traversal.Traversal' a a c d -> d -> m d
-- @
(<.=) :: MonadState a m => Setting a a c d -> d -> m d
l <.= d = do
  l .= d
  return d
{-# INLINE (<.=) #-}

-- | Reify a setter so it can be stored safely in a container.
newtype ReifiedSetter a b c d = ReifySetter { reflectSetter :: Setter a b c d }

-- | @type 'SimpleReifiedSetter' = 'Control.Lens.Type.Simple' 'ReifiedSetter'@
type SimpleReifiedSetter a b = ReifiedSetter a a b b

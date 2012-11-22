{-# LANGUAGE MagicHash #-}
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
-- A @'Setter' s t a b@ is a generalization of 'fmap' from 'Functor'. It allows you to map into a
-- structure and change out the contents, but it isn't strong enough to allow you to
-- enumerate those contents. Starting with @fmap :: 'Functor' f => (a -> b) -> f a -> f b@
-- we monomorphize the type to obtain @(a -> b) -> s -> t@ and then decorate it with 'Data.Functor.Identity.Identity' to obtain
--
-- @type 'Setter' s t a b = (a -> 'Data.Functor.Identity.Identity' b) -> s -> 'Data.Functor.Identity.Identity' t@
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
  , lifted
  -- * Functional Combinators
  , over
  , mapOf
  , set
  , (.~), (%~)
  , (+~), (-~), (*~), (//~), (^~), (^^~), (**~), (||~), (&&~), (<.~), (?~), (<?~)
  -- * State Combinators
  , assign
  , (.=), (%=)
  , (+=), (-=), (*=), (//=), (^=), (^^=), (**=), (||=), (&&=), (<.=), (?=), (<?=)
  , (<~)
  -- * Storing Setters
  , ReifiedSetter(..)
  -- * Setter Internals
  , Setting
  , SimpleSetting
  -- * Simplicity
  , SimpleSetter
  , SimpleReifiedSetter
  -- * Exported for legible error messages
  , Settable
  , Mutator
  ) where

import Control.Lens.Classes
import Control.Lens.Internal
import Control.Lens.Internal.Combinators
import Control.Monad (liftM)
import Control.Monad.State.Class as State

-- $setup
-- >>> import Control.Lens

infixr 4 .~, +~, *~, -~, //~, ^~, ^^~, **~, &&~, ||~, %~, <.~, ?~, <?~
infix  4 .=, +=, *=, -=, //=, ^=, ^^=, **=, &&=, ||=, %=, <.=, ?=, <?=
infixr 2 <~

------------------------------------------------------------------------------
-- Setters
------------------------------------------------------------------------------

-- |
-- The only 'Control.Lens.Type.Lens'-like law that can apply to a 'Setter' @l@ is that
--
-- @'set' l y ('set' l x a) ≡ 'set' l y a@
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
type Setter s t a b = forall f. Settable f => (a -> f b) -> s -> f t

-- |
-- Running a 'Setter' instantiates it to a concrete type.
--
-- When consuming a setter directly to perform a mapping, you can use this type, but most
-- user code will not need to use this type.
--
-- By choosing 'Mutator' rather than 'Data.Functor.Identity.Identity', we get nicer error messages.
type Setting s t a b = (a -> Mutator b) -> s -> Mutator t

-- |
--
-- A Simple Setter is just a 'Setter' that doesn't change the types.
--
-- These are particularly common when talking about monomorphic containers. /e.g./
--
-- @'sets' Data.Text.map :: 'SimpleSetter' 'Data.Text.Internal.Text' 'Char'@
--
-- @type 'SimpleSetter' = 'Control.Lens.Type.Simple' 'Setter'@
type SimpleSetter s a = Setter s s a a

-- |
-- This is a useful alias for use when consuming a 'SimpleSetter'.
--
-- Most user code will never have to use this type.
--
-- @type 'SimpleSetting' m = 'Control.Lens.Type.Simple' 'Setting'@
type SimpleSetting s a = Setting s s a a

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

-- | This setter can be used to modify all of the values in a 'Monad'.
--
-- You sometimes have to use this, rather than 'mapped', because due to
-- temporary insanity 'Functor' is not a superclass of 'Monad'.
--
-- @
-- 'liftM' ≡ 'over' 'lifted'
-- @
--
-- >>> over lifted (+1) [1,2,3]
-- [2,3,4]
lifted :: Monad m => Setter (m a) (m b) a b
lifted = sets liftM
{-# INLINE lifted #-}

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
sets :: ((a -> b) -> s -> t) -> Setter s t a b
sets f g = tainted# (f (untainted# g))
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
-- >>> over mapped (*10) [1,2,3]
-- [10,20,30]
--
-- >>> over _1 show (10,20)
-- ("10",20)
--
-- Another way to view 'over' is to say that it transformers a 'Setter' into a
-- \"semantic editor combinator\".
--
-- @'over' :: 'Setter' s t a b -> (a -> b) -> s -> t@
over :: Setting s t a b -> (a -> b) -> s -> t
over l f = runMutator# (l (mutator# f))
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
-- 'mapOf' :: 'Setter' s t a b      -> (a -> b) -> s -> t
-- 'mapOf' :: 'Control.Lens.Iso.Iso' s t a b         -> (a -> b) -> s -> t
-- 'mapOf' :: 'Control.Lens.Type.Lens' s t a b        -> (a -> b) -> s -> t
-- 'mapOf' :: 'Control.Lens.Traversal.Traversal' s t a b   -> (a -> b) -> s -> t
-- @
mapOf :: Setting s t a b -> (a -> b) -> s -> t
mapOf = over
{-# INLINE mapOf #-}

-- | Replace the target of a 'Control.Lens.Type.Lens' or all of the targets of a 'Setter'
-- or 'Control.Lens.Traversal.Traversal' with a constant value.
--
-- @('<$') ≡ 'set' 'mapped'@
--
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
-- 'set' :: 'Setter' s t a b    -> b -> s -> t
-- 'set' :: 'Control.Lens.Iso.Iso' s t a b       -> b -> s -> t
-- 'set' :: 'Control.Lens.Type.Lens' s t a b      -> b -> s -> t
-- 'set' :: 'Control.Lens.Traversal.Traversal' s t a b -> b -> s -> t
-- @
set :: Setting s t a b -> b -> s -> t
set l b = runMutator# (l (\_ -> Mutator b))
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
-- ('%~') :: 'Setter' s t a b    -> (a -> b) -> s -> t
-- ('%~') :: 'Control.Lens.Iso.Iso' s t a b       -> (a -> b) -> s -> t
-- ('%~') :: 'Control.Lens.Type.Lens' s t a b      -> (a -> b) -> s -> t
-- ('%~') :: 'Control.Lens.Traversal.Traversal' s t a b -> (a -> b) -> s -> t
-- @
(%~) :: Setting s t a b -> (a -> b) -> s -> t
(%~) = over
{-# INLINE (%~) #-}

-- | Replace the target of a 'Control.Lens.Type.Lens' or all of the targets of a 'Setter'
-- or 'Control.Lens.Traversal.Traversal' with a constant value.
--
-- This is an infix version of 'set', provided for consistency with ('.=')
--
-- @f '<$' a ≡ 'mapped' '.~' f '$' a@
--
-- >>> _1 .~ "hello" $ (42,"world")
-- ("hello","world")
--
-- @
-- ('.~') :: 'Setter' s t a b    -> b -> s -> t
-- ('.~') :: 'Control.Lens.Iso.Iso' s t a b       -> b -> s -> t
-- ('.~') :: 'Control.Lens.Type.Lens' s t a b      -> b -> s -> t
-- ('.~') :: 'Control.Lens.Traversal.Traversal' s t a b -> b -> s -> t
-- @
(.~) :: Setting s t a b -> b -> s -> t
(.~) = set
{-# INLINE (.~) #-}

-- | Set the target of a 'Control.Lens.Type.Lens', 'Control.Lens.Traversal.Traversal' or 'Setter' to 'Just' a value.
--
-- @l '?~' t ≡ 'set' l ('Just' t)@
--
-- @
-- ('?~') :: 'Setter' s t a ('Maybe' b)    -> b -> s -> t
-- ('?~') :: 'Control.Lens.Iso.Iso' s t a ('Maybe' b)       -> b -> s -> t
-- ('?~') :: 'Control.Lens.Type.Lens' s t a ('Maybe' b)      -> b -> s -> t
-- ('?~') :: 'Control.Lens.Traversal.Traversal' s t a ('Maybe' b) -> b -> s -> t
-- @
(?~) :: Setting s t a (Maybe b) -> b -> s -> t
l ?~ b = set l (Just b)
{-# INLINE (?~) #-}

-- | Set with pass-through
--
-- This is mostly present for consistency, but may be useful for for chaining assignments
--
-- If you do not need a copy of the intermediate result, then using @l '.~' t@ directly is a good idea.
--
-- >>> _3 <.~ "world" $ ("good","morning","vietnam")
-- ("world",("good","morning","world"))
--
-- >>> import Data.Map as Map
-- >>> _2.at "hello" <.~ Just "world" $ (42,Map.fromList [("goodnight","gracie")])
-- (Just "world",(42,fromList [("goodnight","gracie"),("hello","world")]))
--
-- @
-- ('<.~') :: 'Setter' s t a b    -> b -> s -> (b, t)
-- ('<.~') :: 'Control.Lens.Iso.Iso' s t a b       -> b -> s -> (b, t)
-- ('<.~') :: 'Control.Lens.Type.Lens' s t a b      -> b -> s -> (b, t)
-- ('<.~') :: 'Control.Lens.Traversal.Traversal' s t a b -> b -> s -> (b, t)
-- @
(<.~) :: Setting s t a b -> b -> s -> (b, t)
l <.~ b = \s -> (b, set l b s)
{-# INLINE (<.~) #-}

-- | Set to 'Just' a value with pass-through
--
-- This is mostly present for consistency, but may be useful for for chaining assignments
--
-- If you do not need a copy of the intermediate result, then using @l '?~' d@ directly is a good idea.
--
-- >>> import Data.Map as Map
-- >>> _2.at "hello" <?~ "world" $ (42,Map.fromList [("goodnight","gracie")])
-- ("world",(42,fromList [("goodnight","gracie"),("hello","world")]))
--
-- @
-- ('<?~') :: 'Setter' s t a b    -> ('Maybe' b) -> s -> (b, t)
-- ('<?~') :: 'Control.Lens.Iso.Iso' s t a ('Maybe' b)       -> b -> s -> (b, t)
-- ('<?~') :: 'Control.Lens.Type.Lens' s t a ('Maybe' b)      -> b -> s -> (b, t)
-- ('<?~') :: 'Control.Lens.Traversal.Traversal' s t a ('Maybe' b) -> b -> s -> (b, t)
-- @
(<?~) :: Setting s t a (Maybe b) -> b -> s -> (b, t)
l <?~ b = \s -> (b, set l (Just b) s)
{-# INLINE (<?~) #-}

-- | Increment the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal'
--
-- >>> _1 +~ 1 $ (1,2)
-- (2,2)
--
-- >>> both +~ 2 $ (5,6)
-- (7,8)
--
-- @
-- ('+~') :: Num a => 'Control.Lens.Type.Simple' 'Setter' s a -> a -> s -> s
-- ('+~') :: Num a => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a -> a -> s -> s
-- ('+~') :: Num a => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a -> a -> s -> s
-- ('+~') :: Num a => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> a -> s -> s
-- @
(+~) :: Num a => Setting s t a a -> a -> s -> t
l +~ n = over l (+ n)
{-# INLINE (+~) #-}

-- | Multiply the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal'
--
-- >>> _2 *~ 4 $ (1,2)
-- (1,8)
--
-- >>> mapped *~ 2 $ Just 24
-- Just 48
--
-- @
-- ('*~') :: 'Num' a => 'Control.Lens.Type.Simple' 'Setter' s a -> a -> s -> s
-- ('*~') :: 'Num' a => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a -> a -> s -> s
-- ('*~') :: 'Num' a => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a -> a -> s -> s
-- ('*~') :: 'Num' a => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> a -> s -> s
-- @
(*~) :: Num a => Setting s t a a -> a -> s -> t
l *~ n = over l (* n)
{-# INLINE (*~) #-}

-- | Decrement the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal'
--
-- >>> _1 -~ 2 $ (1,2)
-- (-1,2)
--
-- >>> mapped.mapped -~ 1 $ [[4,5],[6,7]]
-- [[3,4],[5,6]]
--
-- @
-- ('-~') :: 'Num' a => 'Control.Lens.Type.Simple' 'Setter' s a -> a -> s -> s
-- ('-~') :: 'Num' a => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a -> a -> s -> s
-- ('-~') :: 'Num' a => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a -> a -> s -> s
-- ('-~') :: 'Num' a => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> a -> s -> s
-- @
(-~) :: Num a => Setting s t a a -> a -> s -> t
l -~ n = over l (subtract n)
{-# INLINE (-~) #-}

-- | Divide the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal'
--
-- >>> _2 //~ 2 $ ("Hawaii",10)
-- ("Hawaii",5.0)
--
-- @
-- ('//~') :: 'Fractional' a => 'Control.Lens.Type.Simple' 'Setter' s a -> a -> s -> s
-- ('//~') :: 'Fractional' a => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a -> a -> s -> s
-- ('//~') :: 'Fractional' a => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a -> a -> s -> s
-- ('//~') :: 'Fractional' a => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> a -> s -> s
-- @
(//~) :: Fractional s => Setting a b s s -> s -> a -> b
l //~ n = over l (/ n)

-- | Raise the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to a non-negative integral power
--
-- >>> _2 ^~ 2 $ (1,3)
-- (1,9)
--
-- @
-- ('^~') :: ('Num' a, 'Integral' e) => 'Control.Lens.Type.Simple' 'Setter' s a -> e -> s -> s
-- ('^~') :: ('Num' a, 'Integral' e) => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a -> e -> s -> s
-- ('^~') :: ('Num' a, 'Integral' e) => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a -> e -> s -> s
-- ('^~') :: ('Num' a, 'Integral' e) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> e -> s -> s
-- @
(^~) :: (Num a, Integral e) => Setting s t a a -> e -> s -> t
l ^~ n = over l (^ n)
{-# INLINE (^~) #-}

-- | Raise the target(s) of a fractionally valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to an integral power
--
-- >>> _2 ^^~ (-1) $ (1,2)
-- (1,0.5)
--
-- @
-- ('^^~') :: ('Fractional' a, 'Integral' e) => 'Control.Lens.Type.Simple' 'Setter' s a -> e -> s -> s
-- ('^^~') :: ('Fractional' a, 'Integral' e) => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a -> e -> s -> s
-- ('^^~') :: ('Fractional' a, 'Integral' e) => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a -> e -> s -> s
-- ('^^~') :: ('Fractional' a, 'Integral' e) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> e -> s -> s
-- @
--
(^^~) :: (Fractional a, Integral e) => Setting s t a a -> e -> s -> t
l ^^~ n = over l (^^ n)
{-# INLINE (^^~) #-}

-- | Raise the target(s) of a floating-point valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to an arbitrary power.
--
-- >>> _2 **~ pi $ (1,3)
-- (1,31.54428070019754)
--
-- @
-- ('**~') :: 'Floating' a => 'Control.Lens.Type.Simple' 'Setter' s a -> a -> s -> s
-- ('**~') :: 'Floating' a => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a -> a -> s -> s
-- ('**~') :: 'Floating' a => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a -> a -> s -> s
-- ('**~') :: 'Floating' a => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> a -> s -> s
-- @
(**~) :: Floating a => Setting s t a a -> a -> s -> t
l **~ n = over l (** n)
{-# INLINE (**~) #-}

-- | Logically '||' the target(s) of a 'Bool'-valued 'Control.Lens.Type.Lens' or 'Setter'
--
-- >>> both ||~ True $ (False,True)
-- (True,True)
--
-- >>> both ||~ False $ (False,True)
-- (False,True)
--
-- @
-- ('||~') :: 'Control.Lens.Type.Simple' 'Setter' s 'Bool' -> 'Bool' -> s -> s
-- ('||~') :: 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s 'Bool' -> 'Bool' -> s -> s
-- ('||~') :: 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s 'Bool' -> 'Bool' -> s -> s
-- ('||~') :: 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s 'Bool' -> 'Bool' -> s -> s
-- @
(||~):: Setting s t Bool Bool -> Bool -> s -> t
l ||~ n = over l (|| n)
{-# INLINE (||~) #-}

-- | Logically '&&' the target(s) of a 'Bool'-valued 'Control.Lens.Type.Lens' or 'Setter'
--
-- >>> both &&~ True $ (False, True)
-- (False,True)
--
-- >>> both &&~ False $ (False, True)
-- (False,False)
--
-- @
-- ('&&~') :: 'Control.Lens.Type.Simple' 'Setter' s 'Bool' -> 'Bool' -> s -> s
-- ('&&~') :: 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s 'Bool' -> 'Bool' -> s -> s
-- ('&&~') :: 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s 'Bool' -> 'Bool' -> s -> s
-- ('&&~') :: 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s 'Bool' -> 'Bool' -> s -> s
-- @
(&&~) :: Setting s t Bool Bool -> Bool -> s -> t
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
-- 'assign' :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a       -> a -> m ()
-- 'assign' :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a      -> a -> m ()
-- 'assign' :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> a -> m ()
-- 'assign' :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Setter' s a    -> a -> m ()
-- @
assign :: MonadState s m => Setting s s a b -> b -> m ()
assign l b = State.modify (set l b)
{-# INLINE assign #-}

-- | Replace the target of a 'Control.Lens.Type.Lens' or all of the targets of a 'Setter' or 'Control.Lens.Traversal.Traversal' in our monadic
-- state with a new value, irrespective of the old.
--
-- This is an infix version of 'assign'.
--
-- @
-- ('.=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a       -> a -> m ()
-- ('.=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a      -> a -> m ()
-- ('.=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> a -> m ()
-- ('.=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Setter' s a    -> a -> m ()
-- @
--
-- "It puts the state in the monad or it gets the hose again."
(.=) :: MonadState s m => Setting s s a b -> b -> m ()
l .= b = State.modify (l .~ b)
{-# INLINE (.=) #-}

-- | Map over the target of a 'Control.Lens.Type.Lens' or all of the targets of a 'Setter' or 'Control.Lens.Traversal.Traversal' in our monadic state.
--
-- @
-- ('%=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a       -> (a -> a) -> m ()
-- ('%=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a      -> (a -> a) -> m ()
-- ('%=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> (a -> a) -> m ()
-- ('%=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Setter' s a    -> (a -> a) -> m ()
-- @
(%=) :: MonadState s m => Setting s s a b -> (a -> b) -> m ()
l %= f = State.modify (l %~ f)
{-# INLINE (%=) #-}

-- | Replace the target of a 'Control.Lens.Type.Lens' or all of the targets of a 'Setter' or 'Control.Lens.Traversal.Traversal' in our monadic
-- state with 'Just' a new value, irrespective of the old.
--
-- @
-- ('?=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s ('Maybe' a)       -> a -> m ()
-- ('?=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s ('Maybe' a)      -> a -> m ()
-- ('?=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s ('Maybe' a) -> a -> m ()
-- ('?=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Setter' s ('Maybe' a)    -> a -> m ()
-- @
(?=) :: MonadState s m => Setting s s a (Maybe b) -> b -> m ()
l ?= b = State.modify (l ?~ b)
{-# INLINE (?=) #-}

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
-- ('+=') :: ('MonadState' s m, 'Num' a) => 'Control.Lens.Type.Simple' 'Setter' s a -> a -> m ()
-- ('+=') :: ('MonadState' s m, 'Num' a) => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a -> a -> m ()
-- ('+=') :: ('MonadState' s m, 'Num' a) => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a -> a -> m ()
-- ('+=') :: ('MonadState' s m, 'Num' a) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> a -> m ()
-- @
(+=) :: (MonadState s m, Num a) => SimpleSetting s a -> a -> m ()
l += b = State.modify (l +~ b)
{-# INLINE (+=) #-}

-- | Modify the target(s) of a 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal' by subtracting a value
--
-- @
-- ('-=') :: ('MonadState' s m, 'Num' a) => 'Control.Lens.Type.Simple' 'Setter' s a -> a -> m ()
-- ('-=') :: ('MonadState' s m, 'Num' a) => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a -> a -> m ()
-- ('-=') :: ('MonadState' s m, 'Num' a) => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a -> a -> m ()
-- ('-=') :: ('MonadState' s m, 'Num' a) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> a -> m ()
-- @
(-=) :: (MonadState s m, Num a) => SimpleSetting s a -> a -> m ()
l -= b = State.modify (l -~ b)
{-# INLINE (-=) #-}

-- | Modify the target(s) of a 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal' by multiplying by value.
--
-- @
-- ('*=') :: ('MonadState' s m, 'Num' a) => 'Control.Lens.Type.Simple' 'Setter' s a -> a -> m ()
-- ('*=') :: ('MonadState' s m, 'Num' a) => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a -> a -> m ()
-- ('*=') :: ('MonadState' s m, 'Num' a) => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a -> a -> m ()
-- ('*=') :: ('MonadState' s m, 'Num' a) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> a -> m ()
-- @
(*=) :: (MonadState s m, Num a) => SimpleSetting s a -> a -> m ()
l *= b = State.modify (l *~ b)
{-# INLINE (*=) #-}

-- | Modify the target(s) of a 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal' by dividing by a value.
--
-- @
-- ('//=') :: ('MonadState' s m, 'Fractional' a) => 'Control.Lens.Type.Simple' 'Setter' s a -> a -> m ()
-- ('//=') :: ('MonadState' s m, 'Fractional' a) => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a -> a -> m ()
-- ('//=') :: ('MonadState' s m, 'Fractional' a) => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a -> a -> m ()
-- ('//=') :: ('MonadState' s m, 'Fractional' a) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> a -> m ()
-- @
(//=) :: (MonadState s m, Fractional a) => SimpleSetting s a -> a -> m ()
l //= a = State.modify (l //~ a)
{-# INLINE (//=) #-}

-- | Raise the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to a non-negative integral power.
--
-- @
-- ('^=') ::  ('MonadState' s m, 'Num' a, 'Integral' e) => 'Control.Lens.Type.Simple' 'Setter' s a -> e -> m ()
-- ('^=') ::  ('MonadState' s m, 'Num' a, 'Integral' e) => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a -> e -> m ()
-- ('^=') ::  ('MonadState' s m, 'Num' a, 'Integral' e) => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a -> e -> m ()
-- ('^=') ::  ('MonadState' s m, 'Num' a, 'Integral' e) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> e -> m ()
-- @
(^=) :: (MonadState s m, Num a, Integral e) => SimpleSetting s a -> e -> m ()
l ^= e = State.modify (l ^~ e)
{-# INLINE (^=) #-}

-- | Raise the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to an integral power.
--
-- @
-- ('^^=') ::  ('MonadState' s m, 'Fractional' a, 'Integral' e) => 'Control.Lens.Type.Simple' 'Setter' s a -> e -> m ()
-- ('^^=') ::  ('MonadState' s m, 'Fractional' a, 'Integral' e) => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a -> e -> m ()
-- ('^^=') ::  ('MonadState' s m, 'Fractional' a, 'Integral' e) => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a -> e -> m ()
-- ('^^=') ::  ('MonadState' s m, 'Fractional' a, 'Integral' e) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> e -> m ()
-- @
(^^=) :: (MonadState s m, Fractional a, Integral e) => SimpleSetting s a -> e -> m ()
l ^^= e = State.modify (l ^^~ e)
{-# INLINE (^^=) #-}

-- | Raise the target(s) of a numerically valued 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to an arbitrary power
--
-- @
-- ('**=') ::  ('MonadState' s m, 'Floating' a) => 'Control.Lens.Type.Simple' 'Setter' s a -> a -> m ()
-- ('**=') ::  ('MonadState' s m, 'Floating' a) => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a -> a -> m ()
-- ('**=') ::  ('MonadState' s m, 'Floating' a) => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a -> a -> m ()
-- ('**=') ::  ('MonadState' s m, 'Floating' a) => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> a -> m ()
-- @
(**=) :: (MonadState s m, Floating a) => SimpleSetting s a -> a -> m ()
l **= a = State.modify (l **~ a)
{-# INLINE (**=) #-}

-- | Modify the target(s) of a 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens', 'Control.Lens.Iso.Iso', 'Setter' or 'Control.Lens.Traversal.Traversal' by taking their logical '&&' with a value
--
-- @
-- ('&&=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Setter' s 'Bool' -> 'Bool' -> m ()
-- ('&&=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s 'Bool' -> 'Bool' -> m ()
-- ('&&=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s 'Bool' -> 'Bool' -> m ()
-- ('&&=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s 'Bool' -> 'Bool' -> m ()
-- @
(&&=):: MonadState s m => SimpleSetting s Bool -> Bool -> m ()
l &&= b = State.modify (l &&~ b)
{-# INLINE (&&=) #-}

-- | Modify the target(s) of a 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens', 'Iso, 'Setter' or 'Control.Lens.Traversal.Traversal' by taking their logical '||' with a value
--
-- @
-- ('||=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Setter' s 'Bool' -> 'Bool' -> m ()
-- ('||=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s 'Bool' -> 'Bool' -> m ()
-- ('||=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s 'Bool' -> 'Bool' -> m ()
-- ('||=') :: 'MonadState' s m => 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s 'Bool' -> 'Bool' -> m ()
-- @
(||=) :: MonadState s m => SimpleSetting s Bool -> Bool -> m ()
l ||= b = State.modify (l ||~ b)
{-# INLINE (||=) #-}

-- | Run a monadic action, and set all of the targets of a 'Control.Lens.Type.Lens', 'Setter' or 'Control.Lens.Traversal.Traversal' to its result.
--
-- @
-- ('<~') :: 'MonadState' s m => 'Control.Lens.Iso.Iso' s s a b       -> m b -> m ()
-- ('<~') :: 'MonadState' s m => 'Control.Lens.Type.Lens' s s a b      -> m b -> m ()
-- ('<~') :: 'MonadState' s m => 'Control.Lens.Traversal.Traversal' s s a b -> m b -> m ()
-- ('<~') :: 'MonadState' s m => 'Setter' s s a b    -> m b -> m ()
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
(<~) :: MonadState s m => Setting s s a b -> m b -> m ()
l <~ mb = mb >>= (l .=)
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
-- ('<.=') :: 'MonadState' s m => 'Setter' s s a b -> b -> m b
-- ('<.=') :: 'MonadState' s m => 'Control.Lens.Iso.Iso' s s a b -> b -> m b
-- ('<.=') :: 'MonadState' s m => 'Control.Lens.Type.Lens' s s a b -> b -> m b
-- ('<.=') :: 'MonadState' s m => 'Control.Lens.Traversal.Traversal' s s a b -> b -> m b
-- @
(<.=) :: MonadState s m => Setting s s a b -> b -> m b
l <.= b = do
  l .= b
  return b
{-# INLINE (<.=) #-}

-- | Set 'Just' a value with pass-through
--
-- This is useful for chaining assignment without round-tripping through your monad stack.
--
-- @do x <- at "foo" <?= ninety_nine_bottles_of_beer_on_the_wall@
--
-- If you do not need a copy of the intermediate result, then using @l ?= d@ will avoid unused binding warnings
--
-- @
-- ('<?=') :: 'MonadState' s m => 'Setter' s s a ('Maybe' b) -> b -> m b
-- ('<?=') :: 'MonadState' s m => 'Control.Lens.Iso.Iso' s s a ('Maybe' b) -> b -> m b
-- ('<?=') :: 'MonadState' s m => 'Control.Lens.Type.Lens' s s a ('Maybe' b) -> b -> m b
-- ('<?=') :: 'MonadState' s m => 'Control.Lens.Traversal.Traversal' s s a ('Maybe' b) -> b -> m b
-- @
(<?=) :: MonadState s m => Setting s s a (Maybe b) -> b -> m b
l <?= b = do
  l ?= b
  return b
{-# INLINE (<?=) #-}

-- | Reify a setter so it can be stored safely in a container.
newtype ReifiedSetter s t a b = ReifySetter { reflectSetter :: Setter s t a b }

-- | @type 'SimpleReifiedSetter' = 'Control.Lens.Type.Simple' 'ReifiedSetter'@
type SimpleReifiedSetter s a = ReifiedSetter s s a a


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
  , (+~), (-~), (*~), (//~), (^~), (^^~), (**~), (||~), (<>~), (&&~), (<.~), (?~), (<?~)
  -- * State Combinators
  , assign
  , (.=), (%=)
  , (+=), (-=), (*=), (//=), (^=), (^^=), (**=), (||=), (<>=), (&&=), (<.=), (?=), (<?=)
  , (<~)
  -- * Simplified State Setting
  , set'
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
import Data.Monoid

-- $setup
-- >>> import Control.Lens
-- >>> import Control.Monad.State
-- >>> import Data.Map as Map
-- >>> import Debug.SimpleReflect.Expr as Expr
-- >>> import Debug.SimpleReflect.Vars as Vars
-- >>> let f :: Expr -> Expr; f = Vars.f
-- >>> let g :: Expr -> Expr; g = Vars.g
-- >>> let h :: Expr -> Expr -> Expr; h = Vars.h
-- >>> let getter :: Expr -> Expr; getter = fun "getter"
-- >>> let setter :: Expr -> Expr -> Expr; setter = fun "setter"

-- This would be nice to have for the Monoid examples, but adding data types or
-- instances causes doctest on Travis-CI to flip out.
--
-- >>> instance Monoid Expr where mappend = Expr.op InfixR 6 "<>"; mempty = var "mempty"

infixr 4 .~, +~, *~, -~, //~, ^~, ^^~, **~, &&~, <>~, ||~, %~, <.~, ?~, <?~
infix  4 .=, +=, *=, -=, //=, ^=, ^^=, **=, &&=, <>=, ||=, %=, <.=, ?=, <?=
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
--
-- >>> over traverse f [a,b,c,d]
-- [f a,f b,f c,f d]
--
-- >>> over _1 f (a,b)
-- (f a,b)
--
-- >>> over (traverse._1) f [(a,b),(c,d)]
-- [(f a,b),(f c,d)]
--
-- >>> over both f (a,b)
-- (f a,f b)
--
-- >>> over (traverse.both) f [(a,b),(c,d)]
-- [(f a,f b),(f c,f d)]
type Setter s t a b = forall f g. (Settable f, Settable g) => (g a -> f b) -> g s -> f t

-- |
-- Running a 'Setter' instantiates it to a concrete type.
--
-- When consuming a setter directly to perform a mapping, you can use this type, but most
-- user code will not need to use this type.
--
-- By choosing 'Mutator' rather than 'Data.Functor.Identity.Identity', we get nicer error messages.
--
-- FIXME: If Getting is changed to use something other than
-- Mutator, this should probably use it for "g", since g is
-- conceptually an access here.
type Setting s t a b = (Mutator a -> Mutator b) -> Mutator s -> Mutator t

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
-- >>> over mapped f [a,b,c]
-- [f a,f b,f c]
--
-- >>> over mapped (+1) [1,2,3]
-- [2,3,4]
--
-- >>> set mapped x [a,b,c]
-- [x,x,x]
--
-- >>> [[a,b],[c]] & mapped.mapped +~ x
-- [[a + x,b + x],[c + x]]
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
-- >>> over lifted f [a,b,c]
-- [f a,f b,f c]
--
-- >>> set lifted b (Just a)
-- Just b
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
sets f g = point # f (copoint # g # point) # copoint
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
-- Given any valid 'Setter' @l@, you can also rely on the law:
--
-- @'over' l f . 'over' l g = 'over' l (f . g)@
--
-- /e.g./
--
-- >>> over mapped f (over mapped g [a,b,c]) == over mapped (f . g) [a,b,c]
-- True
--
-- Another way to view 'over' is to say that it transforms a 'Setter' into a
-- \"semantic editor combinator\".
--
-- >>> over mapped f (Just a)
-- Just (f a)
--
-- >>> over mapped (*10) [1,2,3]
-- [10,20,30]
--
-- >>> over _1 f (a,b)
-- (f a,b)
--
-- >>> over _1 show (10,20)
-- ("10",20)
--
-- @'over' :: 'Setter' s t a b -> (a -> b) -> s -> t@
over :: Setting s t a b -> (a -> b) -> s -> t
over l f = copoint # l (point # f # copoint) # point
{-# INLINE over #-}

-- | 'mapOf' is a deprecated alias for 'over'.
mapOf :: Setting s t a b -> (a -> b) -> s -> t
mapOf = over
{-# INLINE mapOf #-}
{-# DEPRECATED mapOf "Use `over`" #-}

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
set l b = copoint # l (\_ -> point b) # point
{-# INLINE set #-}

-- |
-- Replace the target of a 'Control.Lens.Type.Lens' or all of the targets of a 'Control.Lens.Type.Simple' 'Setter'
-- or 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' with a constant value, without changing its type.
--
-- This is a type restricted version of 'set', which retains the type of the original.
--
-- >>> set' mapped x [a,b,c,d]
-- [x,x,x,x]
--
-- >>> set' _2 "hello" (1,"world")
-- (1,"hello")
--
-- >>> set' mapped 0 [1,2,3,4]
-- [0,0,0,0]
--
-- Note: Attempting to adjust 'set'' a 'Fold' or 'Getter' will fail at compile time with an
-- relatively nice error message.
--
-- @
-- 'set'' :: 'Control.Lens.Type.Simple' 'Setter' s a    -> a -> s -> s
-- 'set'' :: 'Control.Lens.Type.Simple' 'Control.Lens.Iso.Iso' s a       -> a -> s -> s
-- 'set'' :: 'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens' s a      -> a -> s -> s
-- 'set'' :: 'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal' s a -> a -> s -> s
-- @
set' :: Setting s s a a -> a -> s -> s
set' l b = copoint # l (\_ -> point b) # point
{-# INLINE set' #-}

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
-- >>> (a,b,c) & _3 %~ f
-- (a,b,f c)
--
-- >>> (a,b) & both %~ f
-- (f a,f b)
--
-- >>> _2 %~ length $ (1,"hello")
-- (1,5)
--
-- >>> traverse %~ f $ [a,b,c]
-- [f a,f b,f c]
--
-- >>> traverse %~ even $ [1,2,3]
-- [False,True,False]
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
-- >>> (a,b,c,d) & _4 .~ e
-- (a,b,c,e)
--
-- >>> (42,"world") & _1 .~ "hello"
-- ("hello","world")
--
-- >>> (a,b) & both .~ c
-- (c,c)
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
-- >>> Nothing & id ?~ a
-- Just a
--
-- >>> Map.empty & at 3 ?~ x
-- fromList [(3,x)]
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
-- >>> (a,b) & _1 <.~ c
-- (c,(c,b))
--
-- >>> ("good","morning","vietnam") & _3 <.~ "world"
-- ("world",("good","morning","world"))
--
-- >>> (42,Map.fromList [("goodnight","gracie")]) & _2.at "hello" <.~ Just "world"
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
-- >>> (a,b) & _1 +~ c
-- (a + c,b)
--
-- >>> (a,b) & both +~ c
-- (a + c,b + c)
--
-- >>> (1,2) & _2 +~ 1
-- (1,3)
--
-- >>> [(a,b),(c,d)] & traverse.both +~ e
-- [(a + e,b + e),(c + e,d + e)]
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
-- >>> (a,b) & _1 *~ c
-- (a * c,b)
--
-- >>> (a,b) & both *~ c
-- (a * c,b * c)
--
-- >>> (1,2) & _2 *~ 4
-- (1,8)
--
-- >>> Just 24 & mapped *~ 2
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
-- >>> (a,b) & _1 -~ c
-- (a - c,b)
--
-- >>> (a,b) & both -~ c
-- (a - c,b - c)
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
-- >>> (a,b) & _1 //~ c
-- (a / c,b)
--
-- >>> (a,b) & both //~ c
-- (a / c,b / c)
--
-- >>> ("Hawaii",10) & _2 //~ 2
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
-- >>> (1,3) & _2 ^~ 2
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
-- >>> (1,2) & _2 ^^~ (-1)
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
-- >>> (a,b) & _1 **~ c
-- (a**c,b)
--
-- >>> (a,b) & both **~ c
-- (a**c,b**c)
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
-- >>> execState (do assign _1 c; assign _2 d) (a,b)
-- (c,d)
--
-- >>> execState (both .= c) (a,b)
-- (c,c)
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
-- >>> execState (do _1 .= c; _2 .= d) (a,b)
-- (c,d)
--
-- >>> execState (both .= c) (a,b)
-- (c,c)
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
-- >>> execState (do _1 %= f;_2 %= g) (a,b)
-- (f a,g b)
--
-- >>> execState (do both %= f) (a,b)
-- (f a,f b)
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
-- >>> execState (do at 1 ?= a; at 2 ?= b) Map.empty
-- fromList [(1,a),(2,b)]
--
-- >>> execState (do _1 ?= b; _2 ?= c) (Just a, Nothing)
-- (Just b,Just c)
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
-- >>> execState (do _1 += c; _2 += d) (a,b)
-- (a + c,b + d)
--
-- >>> execState (do _1.at 1.non 0 += 10) (Map.fromList [(2,100)],"hello")
-- (fromList [(1,10),(2,100)],"hello")
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
-- >>> execState (do _1 -= c; _2 -= d) (a,b)
-- (a - c,b - d)
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
-- >>> execState (do _1 *= c; _2 *= d) (a,b)
-- (a * c,b * d)
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
-- >>> execState (do _1 //= c; _2 //= d) (a,b)
-- (a / c,b / d)
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
-- >>> execState (do _1 **= c; _2 **= d) (a,b)
-- (a**c,b**d)
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
-- >>> execState (do _1 &&= True; _2 &&= False; _3 &&= True; _4 &&= False) (True,True,False,False)
-- (True,False,False,False)
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
-- >>> execState (do _1 ||= True; _2 ||= False; _3 ||= True; _4 ||= False) (True,True,False,False)
-- (True,True,True,False)
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

-- | Modify the target of a monoidally valued by 'mappend'ing another value.
--
-- >>> (Sum a,b) & _1 <>~ Sum c
-- (Sum {getSum = a + c},b)
--
-- >>> (Sum a,Sum b) & both <>~ Sum c
-- (Sum {getSum = a + c},Sum {getSum = b + c})
--
-- >>> both <>~ "!!!" $ ("hello","world")
-- ("hello!!!","world!!!")
--
-- @
-- ('<>~') :: 'Monoid' a => 'Setter' s t a a -> a -> s -> t
-- ('<>~') :: 'Monoid' a => 'Iso' s t a a -> a -> s -> t
-- ('<>~') :: 'Monoid' a => 'Lens' s t a a -> a -> s -> t
-- ('<>~') :: 'Monoid' a => 'Traversal' s t a a -> a -> s -> t
-- @
(<>~) :: Monoid a => Setting s t a a -> a -> s -> t
l <>~ n = over l (`mappend` n)
{-# INLINE (<>~) #-}

-- | Modify the target(s) of a 'Control.Lens.Type.Simple' 'Lens', 'Iso', 'Setter' or 'Traversal' by 'mappend'ing a value.
--
-- >>> execState (do _1 <>= Sum c; _2 <>= Product d) (Sum a,Product b)
-- (Sum {getSum = a + c},Product {getProduct = b * d})
--
-- >>> execState (both <>= "!!!") ("hello","world")
-- ("hello!!!","world!!!")
--
-- @
-- ('<>=') :: ('MonadState' s m, 'Monoid' a) => 'Control.Lens.Type.Simple' 'Setter' s a -> a -> m ()
-- ('<>=') :: ('MonadState' s m, 'Monoid' a) => 'Control.Lens.Type.Simple' 'Iso' s a -> a -> m ()
-- ('<>=') :: ('MonadState' s m, 'Monoid' a) => 'Control.Lens.Type.Simple' 'Lens' s a -> a -> m ()
-- ('<>=') :: ('MonadState' s m, 'Monoid' a) => 'Control.Lens.Type.Simple' 'Traversal' s a -> a -> m ()
-- @
(<>=) :: (MonadState s m, Monoid a) => SimpleSetting s a -> a -> m ()
l <>= a = State.modify (l <>~ a)
{-# INLINE (<>=) #-}

-- | Reify a setter so it can be stored safely in a container.
newtype ReifiedSetter s t a b = ReifySetter { reflectSetter :: Setter s t a b }

-- | @type 'SimpleReifiedSetter' = 'Control.Lens.Type.Simple' 'ReifiedSetter'@
type SimpleReifiedSetter s a = ReifiedSetter s s a a

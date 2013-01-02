{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
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
-- Every 'Traversal' is a valid 'Setter', since 'Data.Functor.Identity.Identity' is 'Applicative'.
--
-- Everything you can do with a 'Functor', you can do with a 'Setter'. There
-- are combinators that generalize 'fmap' and ('<$').
----------------------------------------------------------------------------
module Control.Lens.Setter
  (
  -- * Setters
    Setter, Setter'
  , IndexedSetter, IndexedSetter'
  , ASetter, ASetter'
  , AnIndexedSetter, AnIndexedSetter'
  -- * Building Setters
  , sets, setting
  , cloneSetter
  , cloneIndexPreservingSetter
  , cloneIndexedSetter
  -- * Common Setters
  , mapped, lifted
  -- * Functional Combinators
  , over
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
  -- * Indexed Setters
  , imapOf, iover
  , isets
  , (%@~), (%@=)
  -- * Exported for legible error messages
  , Settable
  , Mutator
  -- * Deprecated
  , mapOf
  ) where

import Control.Applicative
import Control.Comonad
import Control.Lens.Internal
import Control.Lens.Type
import Control.Monad (liftM)
import Control.Monad.State.Class as State
import Data.Monoid
import Data.Profunctor
import Data.Profunctor.Corepresentable
import Data.Profunctor.Representable

{-# ANN module "HLint: ignore Avoid lambda" #-}

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

infixr 4 %@~, .~, +~, *~, -~, //~, ^~, ^^~, **~, &&~, <>~, ||~, %~, <.~, ?~, <?~
infix  4 %@=, .=, +=, *=, -=, //=, ^=, ^^=, **=, &&=, <>=, ||=, %=, <.=, ?=, <?=
infixr 2 <~

------------------------------------------------------------------------------
-- Setters
------------------------------------------------------------------------------

-- |
-- Running a 'Setter' instantiates it to a concrete type.
--
-- When consuming a setter directly to perform a mapping, you can use this type, but most
-- user code will not need to use this type.
--
-- By choosing 'Mutator' rather than 'Data.Functor.Identity.Identity', we get nicer error messages.
type ASetter s t a b = (a -> Mutator b) -> s -> Mutator t

-- |
-- This is a useful alias for use when consuming a 'Setter''.
--
-- Most user code will never have to use this type.
--
-- @type 'ASetter'' = 'Simple' 'ASetter'@
type ASetter' s a = ASetter s s a a

-- | Running an 'IndexedSetter' instantiates it to a concrete type.
--
-- When consuming a setter directly to perform a mapping, you can use this type, but most
-- user code will not need to use this type.
--
-- By choosing 'Mutator' rather than 'Data.Functor.Identity.Identity', we get nicer error messages.
type AnIndexedSetter i s t a b = Indexed i a (Mutator b) -> s -> Mutator t

-- | @type 'AnIndexedSetter'' i = 'Simple' ('AnIndexedSetter' i)@
type AnIndexedSetter' i s a = AnIndexedSetter i s s a a

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
mapped :: Functor f => IndexPreservingSetter (f a) (f b) a b
mapped = setting fmap
{-# INLINE mapped #-}

-- | This setter can be used to modify all of the values in a 'Monad'.
--
-- You sometimes have to use this, rather than 'mapped', because due to
-- temporary insanity 'Functor' is not a superclass of 'Monad'.
--
-- @'liftM' ≡ 'over' 'lifted'@
--
-- >>> over lifted f [a,b,c]
-- [f a,f b,f c]
--
-- >>> set lifted b (Just a)
-- Just b
lifted :: Monad m => IndexPreservingSetter (m a) (m b) a b
lifted = setting liftM
{-# INLINE lifted #-}

-- | Build an index-preserving setter from a map-like function.
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
-- 'setting' '.' 'over' ≡ 'id'
-- 'over' '.' 'setting' ≡ 'id'
-- @
--
-- Another way to view 'sets' is that it takes a \"semantic editor combinator\"
-- and transforms it into a 'Setter'.
--
-- @'setting' :: ((a -> b) -> s -> t) -> 'Setter' s t a b@
setting :: ((a -> b) -> s -> t) -> IndexPreservingSetter s t a b
setting l pafb = tabulatePro $ \ws -> pure $ l (\a -> untainted (indexPro pafb (a <$ ws))) (extract ws)
{-# INLINE setting #-}

-- | Build a 'Setter', 'IndexedSetter' or 'IndexPreservingSetter' depending on your choice of profunctor.
--
-- @'sets' :: ((a -> b) -> s -> t) -> 'Setter' s t a b@
sets :: (Profunctor p, Profunctor q, Settable f) => (p a b -> q s t) -> Overloading p q f s t a b
sets f g = pure `rmap` f (rmap untainted g)
{-# INLINE sets #-}

-- | Restore 'ASetter' to a full 'Setter'.
cloneSetter :: ASetter s t a b -> Setter s t a b
cloneSetter l afb = taintedDot $ runMutator #. l (Mutator #. untaintedDot afb)
{-# INLINE cloneSetter #-}

-- | Build an 'IndexPreservingSetter' from any 'Setter'.
cloneIndexPreservingSetter :: ASetter s t a b -> IndexPreservingSetter s t a b
cloneIndexPreservingSetter l pafb = tabulatePro $ \ws ->
    pure . runMutator $ l (\a -> Mutator (untainted (indexPro pafb (a <$ ws)))) (extract ws)
{-# INLINE cloneIndexPreservingSetter #-}

-- | Clone an 'IndexedSetter'.
cloneIndexedSetter :: AnIndexedSetter i s t a b -> IndexedSetter i s t a b
cloneIndexedSetter l pafb = (pure .# runMutator) . l (Indexed $ \i a -> Mutator #. untainted $ indexed pafb i a)
{-# INLINE cloneIndexedSetter #-}

-----------------------------------------------------------------------------
-- Using Setters
-----------------------------------------------------------------------------

-- | Modify the target of a 'Lens' or all the targets of a 'Setter' or 'Traversal'
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
-- @
-- 'over' :: 'Setter' s t a b -> (a -> b) -> s -> t
-- 'over' :: ASetter s t a b -> (a -> b) -> s -> t
-- @
over :: (Profunctor p, Profunctor q) => Overloading p q Mutator s t a b -> p a b -> q s t
over l f = runMutator `rmap` l (rmap Mutator f)
{-# INLINE over #-}

-- | Replace the target of a 'Lens' or all of the targets of a 'Setter'
-- or 'Traversal' with a constant value.
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
-- 'set' :: 'Iso' s t a b       -> b -> s -> t
-- 'set' :: 'Lens' s t a b      -> b -> s -> t
-- 'set' :: 'Traversal' s t a b -> b -> s -> t
-- @
set :: Profunctor q => Overloading (->) q Mutator s t a b -> b -> q s t
set l b = runMutator `rmap` l (\_ -> Mutator b)
{-# INLINE set #-}

-- |
-- Replace the target of a 'Lens' or all of the targets of a 'Setter''
-- or 'Traversal'' with a constant value, without changing its type.
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
-- 'set'' :: 'Setter'' s a    -> a -> s -> s
-- 'set'' :: 'Iso'' s a       -> a -> s -> s
-- 'set'' :: 'Lens'' s a      -> a -> s -> s
-- 'set'' :: 'Traversal'' s a -> a -> s -> s
-- @
set' :: Profunctor q => Overloading' (->) q Mutator s a -> a -> q s s
set' l b = runMutator `rmap` l (\_ -> Mutator b)
{-# INLINE set' #-}

-- | Modifies the target of a 'Lens' or all of the targets of a 'Setter' or
-- 'Traversal' with a user supplied function.
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
-- ('%~') :: 'Iso' s t a b       -> (a -> b) -> s -> t
-- ('%~') :: 'Lens' s t a b      -> (a -> b) -> s -> t
-- ('%~') :: 'Traversal' s t a b -> (a -> b) -> s -> t
-- @
(%~) :: (Profunctor p, Profunctor q) => Overloading p q Mutator s t a b -> p a b -> q s t
(%~) = over
{-# INLINE (%~) #-}

-- | Replace the target of a 'Lens' or all of the targets of a 'Setter'
-- or 'Traversal' with a constant value.
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
-- ('.~') :: 'Iso' s t a b       -> b -> s -> t
-- ('.~') :: 'Lens' s t a b      -> b -> s -> t
-- ('.~') :: 'Traversal' s t a b -> b -> s -> t
-- @
(.~) :: Profunctor q => Overloading (->) q Mutator s t a b -> b -> q s t
(.~) = set
{-# INLINE (.~) #-}

-- | Set the target of a 'Lens', 'Traversal' or 'Setter' to 'Just' a value.
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
-- ('?~') :: 'Iso' s t a ('Maybe' b)       -> b -> s -> t
-- ('?~') :: 'Lens' s t a ('Maybe' b)      -> b -> s -> t
-- ('?~') :: 'Traversal' s t a ('Maybe' b) -> b -> s -> t
-- @
(?~) :: Profunctor q => Overloading (->) q Mutator s t a (Maybe b) -> b -> q s t
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
-- ('<.~') :: 'Iso' s t a b       -> b -> s -> (b, t)
-- ('<.~') :: 'Lens' s t a b      -> b -> s -> (b, t)
-- ('<.~') :: 'Traversal' s t a b -> b -> s -> (b, t)
-- @
(<.~) :: CorepresentableProfunctor q => Overloading (->) q Mutator s t a b -> b -> q s (b, t)
l <.~ b = cotabulatePro $ \s -> (,) b <$> coindexPro (set l b) s
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
-- ('<?~') :: 'Setter' s t a ('Maybe' b)    -> b -> s -> (b, t)
-- ('<?~') :: 'Iso' s t a ('Maybe' b)       -> b -> s -> (b, t)
-- ('<?~') :: 'Lens' s t a ('Maybe' b)      -> b -> s -> (b, t)
-- ('<?~') :: 'Traversal' s t a ('Maybe' b) -> b -> s -> (b, t)
-- @
(<?~) :: CorepresentableProfunctor q => Overloading (->) q Mutator s t a (Maybe b) -> b -> q s (b, t)
l <?~ b = cotabulatePro $ \s -> (,) b <$> coindexPro (set l (Just b)) s
{-# INLINE (<?~) #-}

-- | Increment the target(s) of a numerically valued 'Lens', 'Setter' or 'Traversal'
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
-- ('+~') :: Num a => 'Setter'' s a    -> a -> s -> s
-- ('+~') :: Num a => 'Iso'' s a       -> a -> s -> s
-- ('+~') :: Num a => 'Lens'' s a      -> a -> s -> s
-- ('+~') :: Num a => 'Traversal'' s a -> a -> s -> s
-- @
(+~) :: (Profunctor q, Num a) => Overloading (->) q Mutator s t a a -> a -> q s t
l +~ n = over l (+ n)
{-# INLINE (+~) #-}

-- | Multiply the target(s) of a numerically valued 'Lens', 'Iso', 'Setter' or 'Traversal'
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
-- ('*~') :: 'Num' a => 'Setter'' s a    -> a -> s -> s
-- ('*~') :: 'Num' a => 'Iso'' s a       -> a -> s -> s
-- ('*~') :: 'Num' a => 'Lens'' s a      -> a -> s -> s
-- ('*~') :: 'Num' a => 'Traversal'' s a -> a -> s -> s
-- @
(*~) :: (Profunctor q, Num a) => Overloading (->) q Mutator s t a a -> a -> q s t
l *~ n = over l (* n)
{-# INLINE (*~) #-}

-- | Decrement the target(s) of a numerically valued 'Lens', 'Iso', 'Setter' or 'Traversal'
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
-- ('-~') :: 'Num' a => 'Setter'' s a    -> a -> s -> s
-- ('-~') :: 'Num' a => 'Iso'' s a       -> a -> s -> s
-- ('-~') :: 'Num' a => 'Lens'' s a      -> a -> s -> s
-- ('-~') :: 'Num' a => 'Traversal'' s a -> a -> s -> s
-- @
(-~) :: (Profunctor q, Num a) => Overloading (->) q Mutator s t a a -> a -> q s t
l -~ n = over l (subtract n)
{-# INLINE (-~) #-}

-- | Divide the target(s) of a numerically valued 'Lens', 'Iso', 'Setter' or 'Traversal'
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
-- ('//~') :: 'Fractional' a => 'Setter'' s a    -> a -> s -> s
-- ('//~') :: 'Fractional' a => 'Iso'' s a       -> a -> s -> s
-- ('//~') :: 'Fractional' a => 'Lens'' s a      -> a -> s -> s
-- ('//~') :: 'Fractional' a => 'Traversal'' s a -> a -> s -> s
-- @
(//~) :: (Profunctor q, Fractional a) => Overloading (->) q Mutator s t a a -> a -> q s t
l //~ n = over l (/ n)

-- | Raise the target(s) of a numerically valued 'Lens', 'Setter' or 'Traversal' to a non-negative integral power
--
-- >>> (1,3) & _2 ^~ 2
-- (1,9)
--
-- @
-- ('^~') :: ('Num' a, 'Integral' e) => 'Setter'' s a    -> e -> s -> s
-- ('^~') :: ('Num' a, 'Integral' e) => 'Iso'' s a       -> e -> s -> s
-- ('^~') :: ('Num' a, 'Integral' e) => 'Lens'' s a      -> e -> s -> s
-- ('^~') :: ('Num' a, 'Integral' e) => 'Traversal'' s a -> e -> s -> s
-- @
(^~) :: (Profunctor q, Num a, Integral e) => Overloading (->) q Mutator s t a a -> e -> q s t
l ^~ n = over l (^ n)
{-# INLINE (^~) #-}

-- | Raise the target(s) of a fractionally valued 'Lens', 'Setter' or 'Traversal' to an integral power
--
-- >>> (1,2) & _2 ^^~ (-1)
-- (1,0.5)
--
-- @
-- ('^^~') :: ('Fractional' a, 'Integral' e) => 'Setter'' s a    -> e -> s -> s
-- ('^^~') :: ('Fractional' a, 'Integral' e) => 'Iso'' s a       -> e -> s -> s
-- ('^^~') :: ('Fractional' a, 'Integral' e) => 'Lens'' s a      -> e -> s -> s
-- ('^^~') :: ('Fractional' a, 'Integral' e) => 'Traversal'' s a -> e -> s -> s
-- @
--
(^^~) :: (Profunctor q, Fractional a, Integral e) => Overloading (->) q Mutator s t a a -> e -> q s t
l ^^~ n = over l (^^ n)
{-# INLINE (^^~) #-}

-- | Raise the target(s) of a floating-point valued 'Lens', 'Setter' or 'Traversal' to an arbitrary power.
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
-- ('**~') :: 'Floating' a => 'Setter'' s a    -> a -> s -> s
-- ('**~') :: 'Floating' a => 'Iso'' s a       -> a -> s -> s
-- ('**~') :: 'Floating' a => 'Lens'' s a      -> a -> s -> s
-- ('**~') :: 'Floating' a => 'Traversal'' s a -> a -> s -> s
-- @
(**~) :: (Profunctor q, Floating a) => Overloading (->) q Mutator s t a a -> a -> q s t
l **~ n = over l (** n)
{-# INLINE (**~) #-}

-- | Logically '||' the target(s) of a 'Bool'-valued 'Lens' or 'Setter'
--
-- >>> both ||~ True $ (False,True)
-- (True,True)
--
-- >>> both ||~ False $ (False,True)
-- (False,True)
--
-- @
-- ('||~') :: 'Setter'' s 'Bool'    -> 'Bool' -> s -> s
-- ('||~') :: 'Iso'' s 'Bool'       -> 'Bool' -> s -> s
-- ('||~') :: 'Lens'' s 'Bool'      -> 'Bool' -> s -> s
-- ('||~') :: 'Traversal'' s 'Bool' -> 'Bool' -> s -> s
-- @
(||~):: Profunctor q => Overloading (->) q Mutator s t Bool Bool -> Bool -> q s t
l ||~ n = over l (|| n)
{-# INLINE (||~) #-}

-- | Logically '&&' the target(s) of a 'Bool'-valued 'Lens' or 'Setter'
--
-- >>> both &&~ True $ (False, True)
-- (False,True)
--
-- >>> both &&~ False $ (False, True)
-- (False,False)
--
-- @
-- ('&&~') :: 'Setter'' s 'Bool'    -> 'Bool' -> s -> s
-- ('&&~') :: 'Iso'' s 'Bool'       -> 'Bool' -> s -> s
-- ('&&~') :: 'Lens'' s 'Bool'      -> 'Bool' -> s -> s
-- ('&&~') :: 'Traversal'' s 'Bool' -> 'Bool' -> s -> s
-- @
(&&~) :: Profunctor q => Overloading (->) q Mutator s t Bool Bool -> Bool -> q s t
l &&~ n = over l (&& n)
{-# INLINE (&&~) #-}

------------------------------------------------------------------------------
-- Using Setters with State
------------------------------------------------------------------------------

-- | Replace the target of a 'Lens' or all of the targets of a 'Setter' or 'Traversal' in our monadic
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
-- 'assign' :: 'MonadState' s m => 'Iso'' s a       -> a -> m ()
-- 'assign' :: 'MonadState' s m => 'Lens'' s a      -> a -> m ()
-- 'assign' :: 'MonadState' s m => 'Traversal'' s a -> a -> m ()
-- 'assign' :: 'MonadState' s m => 'Setter'' s a    -> a -> m ()
-- @
assign :: MonadState s m => ASetter s s a b -> b -> m ()
assign l b = State.modify (set l b)
{-# INLINE assign #-}

-- | Replace the target of a 'Lens' or all of the targets of a 'Setter' or 'Traversal' in our monadic
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
-- ('.=') :: 'MonadState' s m => 'Iso'' s a       -> a -> m ()
-- ('.=') :: 'MonadState' s m => 'Lens'' s a      -> a -> m ()
-- ('.=') :: 'MonadState' s m => 'Traversal'' s a -> a -> m ()
-- ('.=') :: 'MonadState' s m => 'Setter'' s a    -> a -> m ()
-- @
--
-- "It puts the state in the monad or it gets the hose again."
(.=) :: MonadState s m => ASetter s s a b -> b -> m ()
l .= b = State.modify (l .~ b)
{-# INLINE (.=) #-}

-- | Map over the target of a 'Lens' or all of the targets of a 'Setter' or 'Traversal' in our monadic state.
--
-- >>> execState (do _1 %= f;_2 %= g) (a,b)
-- (f a,g b)
--
-- >>> execState (do both %= f) (a,b)
-- (f a,f b)
--
-- @
-- ('%=') :: 'MonadState' s m => 'Iso'' s a       -> (a -> a) -> m ()
-- ('%=') :: 'MonadState' s m => 'Lens'' s a      -> (a -> a) -> m ()
-- ('%=') :: 'MonadState' s m => 'Traversal'' s a -> (a -> a) -> m ()
-- ('%=') :: 'MonadState' s m => 'Setter'' s a    -> (a -> a) -> m ()
-- @
--
-- @('%=') :: 'MonadState' s m => 'ASetter' s s a b -> (a -> b) -> m ()@
(%=) :: (Profunctor p, MonadState s m) => Overloading p (->) Mutator s s a b -> p a b -> m ()
l %= f = State.modify (l %~ f)
{-# INLINE (%=) #-}

-- | Replace the target of a 'Lens' or all of the targets of a 'Setter' or 'Traversal' in our monadic
-- state with 'Just' a new value, irrespective of the old.
--
-- >>> execState (do at 1 ?= a; at 2 ?= b) Map.empty
-- fromList [(1,a),(2,b)]
--
-- >>> execState (do _1 ?= b; _2 ?= c) (Just a, Nothing)
-- (Just b,Just c)
--
-- @
-- ('?=') :: 'MonadState' s m => 'Iso'' s ('Maybe' a)       -> a -> m ()
-- ('?=') :: 'MonadState' s m => 'Lens'' s ('Maybe' a)      -> a -> m ()
-- ('?=') :: 'MonadState' s m => 'Traversal'' s ('Maybe' a) -> a -> m ()
-- ('?=') :: 'MonadState' s m => 'Setter'' s ('Maybe' a)    -> a -> m ()
-- @
(?=) :: MonadState s m => ASetter s s a (Maybe b) -> b -> m ()
l ?= b = State.modify (l ?~ b)
{-# INLINE (?=) #-}

-- | Modify the target(s) of a 'Lens'', 'Iso', 'Setter' or 'Traversal' by adding a value
--
-- Example:
--
-- @
-- 'fresh' :: 'MonadState' 'Int' m => m 'Int'
-- 'fresh' = do
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
-- ('+=') :: ('MonadState' s m, 'Num' a) => 'Setter'' s a    -> a -> m ()
-- ('+=') :: ('MonadState' s m, 'Num' a) => 'Iso'' s a       -> a -> m ()
-- ('+=') :: ('MonadState' s m, 'Num' a) => 'Lens'' s a      -> a -> m ()
-- ('+=') :: ('MonadState' s m, 'Num' a) => 'Traversal'' s a -> a -> m ()
-- @
(+=) :: (MonadState s m, Num a) => ASetter' s a -> a -> m ()
l += b = State.modify (l +~ b)
{-# INLINE (+=) #-}

-- | Modify the target(s) of a 'Lens'', 'Iso', 'Setter' or 'Traversal' by subtracting a value
--
-- >>> execState (do _1 -= c; _2 -= d) (a,b)
-- (a - c,b - d)
--
-- @
-- ('-=') :: ('MonadState' s m, 'Num' a) => 'Setter'' s a    -> a -> m ()
-- ('-=') :: ('MonadState' s m, 'Num' a) => 'Iso'' s a       -> a -> m ()
-- ('-=') :: ('MonadState' s m, 'Num' a) => 'Lens'' s a      -> a -> m ()
-- ('-=') :: ('MonadState' s m, 'Num' a) => 'Traversal'' s a -> a -> m ()
-- @
(-=) :: (MonadState s m, Num a) => ASetter' s a -> a -> m ()
l -= b = State.modify (l -~ b)
{-# INLINE (-=) #-}

-- | Modify the target(s) of a 'Lens'', 'Iso', 'Setter' or 'Traversal' by multiplying by value.
--
-- >>> execState (do _1 *= c; _2 *= d) (a,b)
-- (a * c,b * d)
--
-- @
-- ('*=') :: ('MonadState' s m, 'Num' a) => 'Setter'' s a    -> a -> m ()
-- ('*=') :: ('MonadState' s m, 'Num' a) => 'Iso'' s a       -> a -> m ()
-- ('*=') :: ('MonadState' s m, 'Num' a) => 'Lens'' s a      -> a -> m ()
-- ('*=') :: ('MonadState' s m, 'Num' a) => 'Traversal'' s a -> a -> m ()
-- @
(*=) :: (MonadState s m, Num a) => ASetter' s a -> a -> m ()
l *= b = State.modify (l *~ b)
{-# INLINE (*=) #-}

-- | Modify the target(s) of a 'Lens'', 'Iso', 'Setter' or 'Traversal' by dividing by a value.
--
-- >>> execState (do _1 //= c; _2 //= d) (a,b)
-- (a / c,b / d)
--
-- @
-- ('//=') :: ('MonadState' s m, 'Fractional' a) => 'Setter'' s a    -> a -> m ()
-- ('//=') :: ('MonadState' s m, 'Fractional' a) => 'Iso'' s a       -> a -> m ()
-- ('//=') :: ('MonadState' s m, 'Fractional' a) => 'Lens'' s a      -> a -> m ()
-- ('//=') :: ('MonadState' s m, 'Fractional' a) => 'Traversal'' s a -> a -> m ()
-- @
(//=) :: (MonadState s m, Fractional a) => ASetter' s a -> a -> m ()
l //= a = State.modify (l //~ a)
{-# INLINE (//=) #-}

-- | Raise the target(s) of a numerically valued 'Lens', 'Setter' or 'Traversal' to a non-negative integral power.
--
-- @
-- ('^=') ::  ('MonadState' s m, 'Num' a, 'Integral' e) => 'Setter'' s a    -> e -> m ()
-- ('^=') ::  ('MonadState' s m, 'Num' a, 'Integral' e) => 'Iso'' s a       -> e -> m ()
-- ('^=') ::  ('MonadState' s m, 'Num' a, 'Integral' e) => 'Lens'' s a      -> e -> m ()
-- ('^=') ::  ('MonadState' s m, 'Num' a, 'Integral' e) => 'Traversal'' s a -> e -> m ()
-- @
(^=) :: (MonadState s m, Num a, Integral e) => ASetter' s a -> e -> m ()
l ^= e = State.modify (l ^~ e)
{-# INLINE (^=) #-}

-- | Raise the target(s) of a numerically valued 'Lens', 'Setter' or 'Traversal' to an integral power.
--
-- @
-- ('^^=') ::  ('MonadState' s m, 'Fractional' a, 'Integral' e) => 'Setter'' s a    -> e -> m ()
-- ('^^=') ::  ('MonadState' s m, 'Fractional' a, 'Integral' e) => 'Iso'' s a       -> e -> m ()
-- ('^^=') ::  ('MonadState' s m, 'Fractional' a, 'Integral' e) => 'Lens'' s a      -> e -> m ()
-- ('^^=') ::  ('MonadState' s m, 'Fractional' a, 'Integral' e) => 'Traversal'' s a -> e -> m ()
-- @
(^^=) :: (MonadState s m, Fractional a, Integral e) => ASetter' s a -> e -> m ()
l ^^= e = State.modify (l ^^~ e)
{-# INLINE (^^=) #-}

-- | Raise the target(s) of a numerically valued 'Lens', 'Setter' or 'Traversal' to an arbitrary power
--
-- >>> execState (do _1 **= c; _2 **= d) (a,b)
-- (a**c,b**d)
--
-- @
-- ('**=') ::  ('MonadState' s m, 'Floating' a) => 'Setter'' s a    -> a -> m ()
-- ('**=') ::  ('MonadState' s m, 'Floating' a) => 'Iso'' s a       -> a -> m ()
-- ('**=') ::  ('MonadState' s m, 'Floating' a) => 'Lens'' s a      -> a -> m ()
-- ('**=') ::  ('MonadState' s m, 'Floating' a) => 'Traversal'' s a -> a -> m ()
-- @
(**=) :: (MonadState s m, Floating a) => ASetter' s a -> a -> m ()
l **= a = State.modify (l **~ a)
{-# INLINE (**=) #-}

-- | Modify the target(s) of a 'Lens'', 'Iso', 'Setter' or 'Traversal' by taking their logical '&&' with a value
--
-- >>> execState (do _1 &&= True; _2 &&= False; _3 &&= True; _4 &&= False) (True,True,False,False)
-- (True,False,False,False)
--
-- @
-- ('&&=') :: 'MonadState' s m => 'Setter'' s 'Bool'    -> 'Bool' -> m ()
-- ('&&=') :: 'MonadState' s m => 'Iso'' s 'Bool'       -> 'Bool' -> m ()
-- ('&&=') :: 'MonadState' s m => 'Lens'' s 'Bool'      -> 'Bool' -> m ()
-- ('&&=') :: 'MonadState' s m => 'Traversal'' s 'Bool' -> 'Bool' -> m ()
-- @
(&&=):: MonadState s m => ASetter' s Bool -> Bool -> m ()
l &&= b = State.modify (l &&~ b)
{-# INLINE (&&=) #-}

-- | Modify the target(s) of a 'Lens'', 'Iso, 'Setter' or 'Traversal' by taking their logical '||' with a value
--
-- >>> execState (do _1 ||= True; _2 ||= False; _3 ||= True; _4 ||= False) (True,True,False,False)
-- (True,True,True,False)
--
-- @
-- ('||=') :: 'MonadState' s m => 'Setter'' s 'Bool'    -> 'Bool' -> m ()
-- ('||=') :: 'MonadState' s m => 'Iso'' s 'Bool'       -> 'Bool' -> m ()
-- ('||=') :: 'MonadState' s m => 'Lens'' s 'Bool'      -> 'Bool' -> m ()
-- ('||=') :: 'MonadState' s m => 'Traversal'' s 'Bool' -> 'Bool' -> m ()
-- @
(||=) :: MonadState s m => ASetter' s Bool -> Bool -> m ()
l ||= b = State.modify (l ||~ b)
{-# INLINE (||=) #-}

-- | Run a monadic action, and set all of the targets of a 'Lens', 'Setter' or 'Traversal' to its result.
--
-- @
-- ('<~') :: 'MonadState' s m => 'Iso' s s a b       -> m b -> m ()
-- ('<~') :: 'MonadState' s m => 'Lens' s s a b      -> m b -> m ()
-- ('<~') :: 'MonadState' s m => 'Traversal' s s a b -> m b -> m ()
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
-- will store the result in a 'Lens', 'Setter', or 'Traversal'.
(<~) :: MonadState s m => ASetter s s a b -> m b -> m ()
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
-- ('<.=') :: 'MonadState' s m => 'Setter' s s a b    -> b -> m b
-- ('<.=') :: 'MonadState' s m => 'Iso' s s a b       -> b -> m b
-- ('<.=') :: 'MonadState' s m => 'Lens' s s a b      -> b -> m b
-- ('<.=') :: 'MonadState' s m => 'Traversal' s s a b -> b -> m b
-- @
(<.=) :: MonadState s m => ASetter s s a b -> b -> m b
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
-- ('<?=') :: 'MonadState' s m => 'Setter' s s a ('Maybe' b)    -> b -> m b
-- ('<?=') :: 'MonadState' s m => 'Iso' s s a ('Maybe' b)       -> b -> m b
-- ('<?=') :: 'MonadState' s m => 'Lens' s s a ('Maybe' b)      -> b -> m b
-- ('<?=') :: 'MonadState' s m => 'Traversal' s s a ('Maybe' b) -> b -> m b
-- @
(<?=) :: MonadState s m => ASetter s s a (Maybe b) -> b -> m b
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
-- ('<>~') :: 'Monoid' a => 'Setter' s t a a    -> a -> s -> t
-- ('<>~') :: 'Monoid' a => 'Iso' s t a a       -> a -> s -> t
-- ('<>~') :: 'Monoid' a => 'Lens' s t a a      -> a -> s -> t
-- ('<>~') :: 'Monoid' a => 'Traversal' s t a a -> a -> s -> t
-- @
(<>~) :: (Profunctor q, Monoid a) => Overloading (->) q Mutator s t a a -> a -> q s t
l <>~ n = over l (`mappend` n)
{-# INLINE (<>~) #-}

-- | Modify the target(s) of a 'Lens'', 'Iso', 'Setter' or 'Traversal' by 'mappend'ing a value.
--
-- >>> execState (do _1 <>= Sum c; _2 <>= Product d) (Sum a,Product b)
-- (Sum {getSum = a + c},Product {getProduct = b * d})
--
-- >>> execState (both <>= "!!!") ("hello","world")
-- ("hello!!!","world!!!")
--
-- @
-- ('<>=') :: ('MonadState' s m, 'Monoid' a) => 'Setter'' s a -> a -> m ()
-- ('<>=') :: ('MonadState' s m, 'Monoid' a) => 'Iso'' s a -> a -> m ()
-- ('<>=') :: ('MonadState' s m, 'Monoid' a) => 'Lens'' s a -> a -> m ()
-- ('<>=') :: ('MonadState' s m, 'Monoid' a) => 'Traversal'' s a -> a -> m ()
-- @
(<>=) :: (MonadState s m, Monoid a) => ASetter' s a -> a -> m ()
l <>= a = State.modify (l <>~ a)
{-# INLINE (<>=) #-}

-----------------------------------------------------------------------------
-- Indexed Setters
----------------------------------------------------------------------------


-- | Map with index. This is an alias for 'imapOf'.
--
-- When you do not need access to the index, then 'over' is more liberal in what it can accept.
--
-- @
-- 'over' l ≡ 'iover' l '.' 'const'
-- 'iover' l ≡ 'over' l . 'Indexed'
-- @
--
-- @
-- 'iover' :: 'IndexedSetter' i s t a b    -> (i -> a -> b) -> s -> t
-- 'iover' :: 'IndexedLens' i s t a b      -> (i -> a -> b) -> s -> t
-- 'iover' :: 'IndexedTraversal' i s t a b -> (i -> a -> b) -> s -> t
-- @
iover :: Profunctor q => Overloading (Indexed i) q Mutator s t a b -> (i -> a -> b) -> q s t
iover l = over l .# Indexed
{-# INLINE iover #-}

-- | Build an 'IndexedSetter' from an 'imap'-like function.
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
-- 'isets' '.' 'iover' ≡ 'id'
-- 'iover' '.' 'isets' ≡ 'id'
-- @
--
-- Another way to view 'sets' is that it takes a \"semantic editor combinator\"
-- and transforms it into a 'Setter'.
isets :: ((i -> a -> b) -> s -> t) -> IndexedSetter i s t a b
isets f = sets (f . indexed)
{-# INLINE isets #-}

-- | Adjust every target of an 'IndexedSetter', 'IndexedLens' or 'IndexedTraversal'
-- with access to the index.
--
-- @('%@~') ≡ 'imapOf'@
--
-- When you do not need access to the index then ('%@~') is more liberal in what it can accept.
--
-- @l '%~' f ≡ l '%@~' 'const' f@
--
-- @
-- ('%@~') :: 'IndexedSetter' i s t a b    -> (i -> a -> b) -> s -> t
-- ('%@~') :: 'IndexedLens' i s t a b      -> (i -> a -> b) -> s -> t
-- ('%@~') :: 'IndexedTraversal' i s t a b -> (i -> a -> b) -> s -> t
-- @
(%@~) :: Profunctor q => Overloading (Indexed i) q Mutator s t a b -> (i -> a -> b) -> q s t
l %@~ f = l %~ Indexed f
{-# INLINE (%@~) #-}

-- | Adjust every target in the current state of an 'IndexedSetter', 'IndexedLens' or 'IndexedTraversal'
-- with access to the index.
--
-- When you do not need access to the index then ('%=') is more liberal in what it can accept.
--
-- @l '%=' f ≡ l '%@=' 'const' f@
--
-- @
-- ('%@=') :: 'MonadState' s m => 'IndexedSetter' i s s a b    -> (i -> a -> b) -> m ()
-- ('%@=') :: 'MonadState' s m => 'IndexedLens' i s s a b      -> (i -> a -> b) -> m ()
-- ('%@=') :: 'MonadState' s m => 'IndexedTraversal' i s t a b -> (i -> a -> b) -> m ()
-- @
(%@=) :: MonadState s m => AnIndexedSetter i s s a b -> (i -> a -> b) -> m ()
l %@= f = State.modify (l %@~ f)
{-# INLINE (%@=) #-}

------------------------------------------------------------------------------
-- Deprecated
------------------------------------------------------------------------------

-- | 'mapOf' is a deprecated alias for 'over'.
mapOf :: (Profunctor p, Profunctor q) => Overloading p q Mutator s t a b -> p a b -> q s t
mapOf = over
{-# INLINE mapOf #-}
{-# DEPRECATED mapOf "Use `over`" #-}


-- | Map with index. (Deprecated alias for 'iover')
--
-- When you do not need access to the index, then 'mapOf' is more liberal in what it can accept.
--
-- @'mapOf' l ≡ 'imapOf' l '.' 'const'@
--
-- @
-- 'imapOf' :: 'IndexedSetter' i s t a b    -> (i -> a -> b) -> s -> t
-- 'imapOf' :: 'IndexedLens' i s t a b      -> (i -> a -> b) -> s -> t
-- 'imapOf' :: 'IndexedTraversal' i s t a b -> (i -> a -> b) -> s -> t
-- @
imapOf :: Profunctor q => Overloading (Indexed i) q Mutator s t a b -> (i -> a -> b) -> q s t
imapOf = iover
{-# INLINE imapOf #-}
{-# DEPRECATED imapOf "Use `iover`" #-}

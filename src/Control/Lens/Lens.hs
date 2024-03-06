{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE PolyKinds #-}
#else
{-# LANGUAGE TypeInType #-}
#endif

#include "lens-common.h"

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- A @'Lens' s t a b@ is a purely functional reference.
--
-- While a 'Control.Lens.Traversal.Traversal' could be used for
-- 'Control.Lens.Getter.Getting' like a valid 'Control.Lens.Fold.Fold', it
-- wasn't a valid 'Control.Lens.Getter.Getter' as a
-- 'Control.Lens.Getter.Getter' can't require an 'Applicative' constraint.
--
-- 'Functor', however, is a constraint on both.
--
-- @
-- type 'Lens' s t a b = forall f. 'Functor' f => (a -> f b) -> s -> f t
-- @
--
-- Every 'Lens' is a valid 'Control.Lens.Setter.Setter'.
--
-- Every 'Lens' can be used for 'Control.Lens.Getter.Getting' like a
-- 'Control.Lens.Fold.Fold' that doesn't use the 'Applicative' or
-- 'Contravariant'.
--
-- Every 'Lens' is a valid 'Control.Lens.Traversal.Traversal' that only uses
-- the 'Functor' part of the 'Applicative' it is supplied.
--
-- Every 'Lens' can be used for 'Control.Lens.Getter.Getting' like a valid
-- 'Control.Lens.Getter.Getter'.
--
-- Since every 'Lens' can be used for 'Control.Lens.Getter.Getting' like a
-- valid 'Control.Lens.Getter.Getter' it follows that it must view exactly one element in the
-- structure.
--
-- The 'Lens' laws follow from this property and the desire for it to act like
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
  , IndexedLens, IndexedLens'
  -- ** Concrete Lenses
  , ALens, ALens'
  , AnIndexedLens, AnIndexedLens'

  -- * Combinators
  , lens, ilens, iplens, withLens
  , (%%~), (%%=)
  , (%%@~), (%%@=)
  , (<%@~), (<%@=)
  , (<<%@~), (<<%@=)
  -- ** General Purpose Combinators
  , (&), (<&>), (??)
  , (&~)
  -- * Lateral Composition
  , choosing
  , chosen
  , alongside
  , inside

  -- * Setting Functionally with Passthrough
  , (<%~), (<+~), (<-~), (<*~), (<//~)
  , (<^~), (<^^~), (<**~)
  , (<||~), (<&&~), (<<>~), (<<>:~)
  , (<<%~), (<<.~), (<<?~), (<<+~), (<<-~), (<<*~)
  , (<<//~), (<<^~), (<<^^~), (<<**~)
  , (<<||~), (<<&&~), (<<<>~)

  -- * Setting State with Passthrough
  , (<%=), (<+=), (<-=), (<*=), (<//=)
  , (<^=), (<^^=), (<**=)
  , (<||=), (<&&=), (<<>=), (<<>:=)
  , (<<%=), (<<.=), (<<?=), (<<+=), (<<-=), (<<*=)
  , (<<//=), (<<^=), (<<^^=), (<<**=)
  , (<<||=), (<<&&=), (<<<>=)
  , (<<~)

  -- * Cloning Lenses
  , cloneLens
  , cloneIndexPreservingLens
  , cloneIndexedLens

  -- * Arrow operators
  , overA

  -- * ALens Combinators
  , storing
  , (^#)
  , (#~), (#%~), (#%%~), (<#~), (<#%~)
  , (#=), (#%=), (#%%=), (<#=), (<#%=)

  -- * Common Lenses
  , devoid
  , united
  , head1, last1

  -- * Context
  , Context(..)
  , Context'
  , locus

  -- * Lens fusion
  , fusing
  ) where

import Prelude ()

import Control.Arrow
import Control.Comonad
import Control.Lens.Internal.Context
import Control.Lens.Internal.Prelude
import Control.Lens.Internal.Getter
import Control.Lens.Internal.Indexed
import Control.Lens.Type
import Control.Monad.State as State
import Data.Functor.Apply
import Data.Functor.Reverse
import Data.Functor.Yoneda
import Data.Semigroup.Traversable
import GHC.Exts (TYPE)

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import Control.Arrow
-- >>> import Control.Monad.State
-- >>> import Data.Char (chr)
-- >>> import Data.List.NonEmpty (NonEmpty ((:|)))
-- >>> import Data.Monoid (Sum (..))
-- >>> import Data.Tree (Tree (Node))
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g,h)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g
-- >>> let h :: Expr -> Expr -> Expr; h = Debug.SimpleReflect.Vars.h
-- >>> let getter :: Expr -> Expr; getter = fun "getter"
-- >>> let setter :: Expr -> Expr -> Expr; setter = fun "setter"

infixl 8 ^#
infixr 4 %%@~, <%@~, <<%@~, %%~, <+~, <*~, <-~, <//~, <^~, <^^~, <**~, <&&~, <||~, <<>~, <<>:~, <%~, <<%~, <<.~, <<?~, <#~, #~, #%~, <#%~, #%%~
       , <<+~, <<-~, <<*~, <<//~, <<^~, <<^^~, <<**~, <<||~, <<&&~, <<<>~
infix  4 %%@=, <%@=, <<%@=, %%=, <+=, <*=, <-=, <//=, <^=, <^^=, <**=, <&&=, <||=, <<>=, <<>:=, <%=, <<%=, <<.=, <<?=, <#=, #=, #%=, <#%=, #%%=
       , <<+=, <<-=, <<*=, <<//=, <<^=, <<^^=, <<**=, <<||=, <<&&=, <<<>=
infixr 2 <<~
infixl 1 ??, &~

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

-- | When you see this as an argument to a function, it expects a 'Lens'.
--
-- This type can also be used when you need to store a 'Lens' in a container,
-- since it is rank-1. You can turn them back into a 'Lens' with 'cloneLens',
-- or use it directly with combinators like 'storing' and ('^#').
type ALens s t a b = LensLike (Pretext (->) a b) s t a b

-- | @
-- type 'ALens'' = 'Simple' 'ALens'
-- @
type ALens' s a = ALens s s a a

-- | When you see this as an argument to a function, it expects an 'IndexedLens'
type AnIndexedLens i s t a b = Optical (Indexed i) (->) (Pretext (Indexed i) a b) s t a b

-- | @
-- type 'AnIndexedLens'' = 'Simple' ('AnIndexedLens' i)
-- @
type AnIndexedLens' i s a  = AnIndexedLens i s s a a

--------------------------
-- Constructing Lenses
--------------------------

-- | Build a 'Lens' from a getter and a setter.
--
-- @
-- 'lens' :: 'Functor' f => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
-- @
--
-- >>> s ^. lens getter setter
-- getter s
--
-- >>> s & lens getter setter .~ b
-- setter s b
--
-- >>> s & lens getter setter %~ f
-- setter s (f (getter s))
--
-- @
-- 'lens' :: (s -> a) -> (s -> a -> s) -> 'Lens'' s a
-- @
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

-- | Obtain a getter and a setter from a lens, reversing 'lens'.
withLens :: forall s t a b rep (r :: TYPE rep).
            ALens s t a b -> ((s -> a) -> (s -> b -> t) -> r) -> r
withLens l f = f (^# l) (flip (storing l))
{-# INLINE withLens #-}

-- | Build an index-preserving 'Lens' from a 'Control.Lens.Getter.Getter' and a
-- 'Control.Lens.Setter.Setter'.
iplens :: (s -> a) -> (s -> b -> t) -> IndexPreservingLens s t a b
iplens sa sbt pafb = cotabulate $ \ws -> sbt (extract ws) <$> cosieve pafb (sa <$> ws)
{-# INLINE iplens #-}

-- | Build an 'IndexedLens' from a 'Control.Lens.Getter.Getter' and
-- a 'Control.Lens.Setter.Setter'.
ilens :: (s -> (i, a)) -> (s -> b -> t) -> IndexedLens i s t a b
ilens sia sbt iafb s = sbt s <$> uncurry (indexed iafb) (sia s)
{-# INLINE ilens #-}

-- | This can be used to chain lens operations using @op=@ syntax
-- rather than @op~@ syntax for simple non-type-changing cases.
--
-- >>> (10,20) & _1 .~ 30 & _2 .~ 40
-- (30,40)
--
-- >>> (10,20) &~ do _1 .= 30; _2 .= 40
-- (30,40)
--
-- This does not support type-changing assignment, /e.g./
--
-- >>> (10,20) & _1 .~ "hello"
-- ("hello",20)
(&~) :: s -> State s a -> s
s &~ l = execState l s
{-# INLINE (&~) #-}

-- | ('%%~') can be used in one of two scenarios:
--
-- When applied to a 'Lens', it can edit the target of the 'Lens' in a
-- structure, extracting a functorial result.
--
-- When applied to a 'Traversal', it can edit the
-- targets of the traversals, extracting an applicative summary of its
-- actions.
--
-- >>> [66,97,116,109,97,110] & each %%~ \a -> ("na", chr a)
-- ("nananananana","Batman")
--
-- For all that the definition of this combinator is just:
--
-- @
-- ('%%~') ≡ 'id'
-- @
--
-- It may be beneficial to think about it as if it had these even more
-- restricted types, however:
--
-- @
-- ('%%~') :: 'Functor' f =>     'Control.Lens.Iso.Iso' s t a b       -> (a -> f b) -> s -> f t
-- ('%%~') :: 'Functor' f =>     'Lens' s t a b      -> (a -> f b) -> s -> f t
-- ('%%~') :: 'Applicative' f => 'Control.Lens.Traversal.Traversal' s t a b -> (a -> f b) -> s -> f t
-- @
--
-- When applied to a 'Traversal', it can edit the
-- targets of the traversals, extracting a supplemental monoidal summary
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
-- @
-- ('%%=') ≡ ('state' '.')
-- @
--
-- It may be useful to think of ('%%='), instead, as having either of the
-- following more restricted type signatures:
--
-- @
-- ('%%=') :: 'MonadState' s m             => 'Control.Lens.Iso.Iso' s s a b       -> (a -> (r, b)) -> m r
-- ('%%=') :: 'MonadState' s m             => 'Lens' s s a b      -> (a -> (r, b)) -> m r
-- ('%%=') :: ('MonadState' s m, 'Monoid' r) => 'Control.Lens.Traversal.Traversal' s s a b -> (a -> (r, b)) -> m r
-- @
(%%=) :: MonadState s m => Over p ((,) r) s s a b -> p a (r, b) -> m r
l %%= f = State.state (l f)
{-# INLINE (%%=) #-}

-------------------------------------------------------------------------------
-- General Purpose Combinators
-------------------------------------------------------------------------------

-- | This is convenient to 'flip' argument order of composite functions defined as:
--
-- @
-- fab ?? a = fmap ($ a) fab
-- @
--
-- For the 'Functor' instance @f = ((->) r)@ you can reason about this function as if the definition was @('??') ≡ 'flip'@:
--
-- >>> (h ?? x) a
-- h a x
--
-- >>> execState ?? [] $ modify (1:)
-- [1]
--
-- >>> over _2 ?? ("hello","world") $ length
-- ("hello",5)
--
-- >>> over ?? length ?? ("hello","world") $ _2
-- ("hello",5)
(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab
{-# INLINE (??) #-}

-------------------------------------------------------------------------------
-- Common Lenses
-------------------------------------------------------------------------------

-- | Lift a 'Lens' so it can run under a function (or other corepresentable profunctor).
--
-- @
-- 'inside' :: 'Lens' s t a b -> 'Lens' (e -> s) (e -> t) (e -> a) (e -> b)
-- @
--
--
-- >>> (\x -> (x-1,x+1)) ^. inside _1 $ 5
-- 4
--
-- >>> runState (modify (1:) >> modify (2:)) ^. (inside _2) $ []
-- [2,1]
inside :: Corepresentable p => ALens s t a b -> Lens (p e s) (p e t) (p e a) (p e b)
inside l f es = o <$> f i where
  i = cotabulate $ \ e -> ipos $ l sell (cosieve es e)
  o ea = cotabulate $ \ e -> ipeek (cosieve ea e) $ l sell (cosieve es e)
{-# INLINE inside #-}

{-
-- | Lift a 'Lens' so it can run under a function (or any other corepresentable functor).
insideF :: F.Representable f => ALens s t a b -> Lens (f s) (f t) (f a) (f b)
insideF l f es = o <$> f i where
  i = F.tabulate $ \e -> ipos $ l sell (F.index es e)
  o ea = F.tabulate $ \ e -> ipeek (F.index ea e) $ l sell (F.index es e)
{-# INLINE inside #-}
-}

-- | Merge two lenses, getters, setters, folds or traversals.
--
-- @
-- 'chosen' ≡ 'choosing' 'id' 'id'
-- @
--
-- @
-- 'choosing' :: 'Control.Lens.Getter.Getter' s a     -> 'Control.Lens.Getter.Getter' s' a     -> 'Control.Lens.Getter.Getter' ('Either' s s') a
-- 'choosing' :: 'Control.Lens.Fold.Fold' s a       -> 'Control.Lens.Fold.Fold' s' a       -> 'Control.Lens.Fold.Fold' ('Either' s s') a
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
-- @
-- 'chosen' ≡ 'choosing' 'id' 'id'
-- @
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
--
-- @
-- 'chosen' :: 'Lens' ('Either' a a) ('Either' b b) a b
-- 'chosen' f ('Left' a)  = 'Left' '<$>' f a
-- 'chosen' f ('Right' a) = 'Right' '<$>' f a
-- @
chosen :: IndexPreservingLens (Either a a) (Either b b) a b
chosen pafb = cotabulate $ \weaa -> cosieve (either id id `lmap` pafb) weaa <&> \b -> case extract weaa of
  Left _  -> Left  b
  Right _ -> Right b
{-# INLINE chosen #-}

-- | 'alongside' makes a 'Lens' from two other lenses or a 'Getter' from two other getters
-- by executing them on their respective halves of a product.
--
-- >>> (Left a, Right b)^.alongside chosen chosen
-- (a,b)
--
-- >>> (Left a, Right b) & alongside chosen chosen .~ (c,d)
-- (Left c,Right d)
--
-- @
-- 'alongside' :: 'Lens'   s t a b -> 'Lens'   s' t' a' b' -> 'Lens'   (s,s') (t,t') (a,a') (b,b')
-- 'alongside' :: 'Getter' s   a   -> 'Getter' s'    a'    -> 'Getter' (s,s')        (a,a')
-- @
alongside :: LensLike (AlongsideLeft f b') s  t  a  b
          -> LensLike (AlongsideRight f t) s' t' a' b'
          -> LensLike f (s, s') (t, t') (a, a') (b, b')
alongside l1 l2 f (a1, a2)
  = getAlongsideRight $ l2 ?? a2 $ \b2 -> AlongsideRight
  $ getAlongsideLeft  $ l1 ?? a1 $ \b1 -> AlongsideLeft
  $ f (b1,b2)
{-# INLINE alongside #-}

-- | This 'Lens' lets you 'view' the current 'pos' of any indexed
-- store comonad and 'seek' to a new position. This reduces the API
-- for working these instances to a single 'Lens'.
--
-- @
-- 'ipos' w ≡ w 'Control.Lens.Getter.^.' 'locus'
-- 'iseek' s w ≡ w '&' 'locus' 'Control.Lens.Setter..~' s
-- 'iseeks' f w ≡ w '&' 'locus' 'Control.Lens.Setter.%~' f
-- @
--
-- @
-- 'locus' :: 'Lens'' ('Context'' a s) a
-- 'locus' :: 'Conjoined' p => 'Lens'' ('Pretext'' p a s) a
-- 'locus' :: 'Conjoined' p => 'Lens'' ('PretextT'' p g a s) a
-- @
locus :: IndexedComonadStore p => Lens (p a c s) (p b c s) a b
locus f w = (`iseek` w) <$> f (ipos w)
{-# INLINE locus #-}

-------------------------------------------------------------------------------
-- Cloning Lenses
-------------------------------------------------------------------------------

-- | Cloning a 'Lens' is one way to make sure you aren't given
-- something weaker, such as a 'Control.Lens.Traversal.Traversal' and can be
-- used as a way to pass around lenses that have to be monomorphic in @f@.
--
-- Note: This only accepts a proper 'Lens'.
--
-- >>> let example l x = set (cloneLens l) (x^.cloneLens l + 1) x in example _2 ("hello",1,"you")
-- ("hello",2,"you")
cloneLens :: ALens s t a b -> Lens s t a b
cloneLens l afb s = runPretext (l sell s) afb
{-# INLINE cloneLens #-}

-- | Clone a 'Lens' as an 'IndexedPreservingLens' that just passes through whatever
-- index is on any 'IndexedLens', 'IndexedFold', 'IndexedGetter' or  'IndexedTraversal' it is composed with.
cloneIndexPreservingLens :: ALens s t a b -> IndexPreservingLens s t a b
cloneIndexPreservingLens l pafb = cotabulate $ \ws -> runPretext (l sell (extract ws)) $ \a -> cosieve pafb (a <$ ws)
{-# INLINE cloneIndexPreservingLens #-}

-- | Clone an 'IndexedLens' as an 'IndexedLens' with the same index.
cloneIndexedLens :: AnIndexedLens i s t a b -> IndexedLens i s t a b
cloneIndexedLens l f s = runPretext (l sell s) (Indexed (indexed f))
{-# INLINE cloneIndexedLens #-}

-------------------------------------------------------------------------------
-- Setting and Remembering
-------------------------------------------------------------------------------

-- | Modify the target of a 'Lens' and return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.%~') is more flexible.
--
-- @
-- ('<%~') ::             'Lens' s t a b      -> (a -> b) -> s -> (b, t)
-- ('<%~') ::             'Control.Lens.Iso.Iso' s t a b       -> (a -> b) -> s -> (b, t)
-- ('<%~') :: 'Monoid' b => 'Control.Lens.Traversal.Traversal' s t a b -> (a -> b) -> s -> (b, t)
-- @
(<%~) :: LensLike ((,) b) s t a b -> (a -> b) -> s -> (b, t)
l <%~ f = l $ (\t -> (t, t)) . f
{-# INLINE (<%~) #-}

-- | Increment the target of a numerically valued 'Lens' and return the result.
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

-- | Decrement the target of a numerically valued 'Lens' and return the result.
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

-- | Multiply the target of a numerically valued 'Lens' and return the result.
--
-- When you do not need the result of the multiplication, ('Control.Lens.Setter.*~') is more
-- flexible.
--
-- @
-- ('<*~') :: 'Num' a => 'Lens'' s a -> a -> s -> (a, s)
-- ('<*~') :: 'Num' a => 'Control.Lens.Iso.Iso''  s a -> a -> s -> (a, s)
-- @
(<*~) :: Num a => LensLike ((,)a) s t a a -> a -> s -> (a, t)
l <*~ a = l <%~ (* a)
{-# INLINE (<*~) #-}

-- | Divide the target of a fractionally valued 'Lens' and return the result.
--
-- When you do not need the result of the division, ('Control.Lens.Setter.//~') is more flexible.
--
-- @
-- ('<//~') :: 'Fractional' a => 'Lens'' s a -> a -> s -> (a, s)
-- ('<//~') :: 'Fractional' a => 'Control.Lens.Iso.Iso''  s a -> a -> s -> (a, s)
-- @
(<//~) :: Fractional a => LensLike ((,)a) s t a a -> a -> s -> (a, t)
l <//~ a = l <%~ (/ a)
{-# INLINE (<//~) #-}

-- | Raise the target of a numerically valued 'Lens' to a non-negative
-- 'Integral' power and return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.^~') is more flexible.
--
-- @
-- ('<^~') :: ('Num' a, 'Integral' e) => 'Lens'' s a -> e -> s -> (a, s)
-- ('<^~') :: ('Num' a, 'Integral' e) => 'Control.Lens.Iso.Iso'' s a -> e -> s -> (a, s)
-- @
(<^~) :: (Num a, Integral e) => LensLike ((,)a) s t a a -> e -> s -> (a, t)
l <^~ e = l <%~ (^ e)
{-# INLINE (<^~) #-}

-- | Raise the target of a fractionally valued 'Lens' to an 'Integral' power
-- and return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.^^~') is more flexible.
--
-- @
-- ('<^^~') :: ('Fractional' a, 'Integral' e) => 'Lens'' s a -> e -> s -> (a, s)
-- ('<^^~') :: ('Fractional' a, 'Integral' e) => 'Control.Lens.Iso.Iso'' s a -> e -> s -> (a, s)
-- @
(<^^~) :: (Fractional a, Integral e) => LensLike ((,)a) s t a a -> e -> s -> (a, t)
l <^^~ e = l <%~ (^^ e)
{-# INLINE (<^^~) #-}

-- | Raise the target of a floating-point valued 'Lens' to an arbitrary power
-- and return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.**~') is more flexible.
--
-- @
-- ('<**~') :: 'Floating' a => 'Lens'' s a -> a -> s -> (a, s)
-- ('<**~') :: 'Floating' a => 'Control.Lens.Iso.Iso'' s a  -> a -> s -> (a, s)
-- @
(<**~) :: Floating a => LensLike ((,)a) s t a a -> a -> s -> (a, t)
l <**~ a = l <%~ (** a)
{-# INLINE (<**~) #-}

-- | Logically '||' a Boolean valued 'Lens' and return the result.
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

-- | Logically '&&' a Boolean valued 'Lens' and return the result.
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
-- When you do not need the old value, ('Control.Lens.Setter.%~') is more flexible.
--
-- @
-- ('<<%~') ::             'Lens' s t a b      -> (a -> b) -> s -> (a, t)
-- ('<<%~') ::             'Control.Lens.Iso.Iso' s t a b       -> (a -> b) -> s -> (a, t)
-- ('<<%~') :: 'Monoid' a => 'Control.Lens.Traversal.Traversal' s t a b -> (a -> b) -> s -> (a, t)
-- @
(<<%~) :: LensLike ((,)a) s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l = l . lmap (\a -> (a, a)) . second'
{-# INLINE (<<%~) #-}

-- | Replace the target of a 'Lens', but return the old value.
--
-- When you do not need the old value, ('Control.Lens.Setter..~') is more flexible.
--
-- @
-- ('<<.~') ::             'Lens' s t a b      -> b -> s -> (a, t)
-- ('<<.~') ::             'Control.Lens.Iso.Iso' s t a b       -> b -> s -> (a, t)
-- ('<<.~') :: 'Monoid' a => 'Control.Lens.Traversal.Traversal' s t a b -> b -> s -> (a, t)
-- @
(<<.~) :: LensLike ((,)a) s t a b -> b -> s -> (a, t)
l <<.~ b = l $ \a -> (a, b)
{-# INLINE (<<.~) #-}

-- | Replace the target of a 'Lens' with a 'Just' value, but return the old value.
--
-- If you do not need the old value ('Control.Lens.Setter.?~') is more flexible.
--
-- >>> import qualified Data.Map as Map
-- >>> _2.at "hello" <<?~ "world" $ (42,Map.fromList [("goodnight","gracie")])
-- (Nothing,(42,fromList [("goodnight","gracie"),("hello","world")]))
--
-- @
-- ('<<?~') :: 'Iso' s t a ('Maybe' b)       -> b -> s -> (a, t)
-- ('<<?~') :: 'Lens' s t a ('Maybe' b)      -> b -> s -> (a, t)
-- ('<<?~') :: 'Traversal' s t a ('Maybe' b) -> b -> s -> (a, t)
-- @
(<<?~) :: LensLike ((,)a) s t a (Maybe b) -> b -> s -> (a, t)
l <<?~ b = l <<.~ Just b
{-# INLINE (<<?~) #-}

-- | Increment the target of a numerically valued 'Lens' and return the old value.
--
-- When you do not need the old value, ('Control.Lens.Setter.+~') is more flexible.
--
-- >>> (a,b) & _1 <<+~ c
-- (a,(a + c,b))
--
-- >>> (a,b) & _2 <<+~ c
-- (b,(a,b + c))
--
-- @
-- ('<<+~') :: 'Num' a => 'Lens'' s a -> a -> s -> (a, s)
-- ('<<+~') :: 'Num' a => 'Iso'' s a -> a -> s -> (a, s)
-- @
(<<+~) :: Num a => LensLike' ((,) a) s a -> a -> s -> (a, s)
l <<+~ b = l $ \a -> (a, a + b)
{-# INLINE (<<+~) #-}

-- | Decrement the target of a numerically valued 'Lens' and return the old value.
--
-- When you do not need the old value, ('Control.Lens.Setter.-~') is more flexible.
--
-- >>> (a,b) & _1 <<-~ c
-- (a,(a - c,b))
--
-- >>> (a,b) & _2 <<-~ c
-- (b,(a,b - c))
--
-- @
-- ('<<-~') :: 'Num' a => 'Lens'' s a -> a -> s -> (a, s)
-- ('<<-~') :: 'Num' a => 'Iso'' s a -> a -> s -> (a, s)
-- @
(<<-~) :: Num a => LensLike' ((,) a) s a -> a -> s -> (a, s)
l <<-~ b = l $ \a -> (a, a - b)
{-# INLINE (<<-~) #-}

-- | Multiply the target of a numerically valued 'Lens' and return the old value.
--
-- When you do not need the old value, ('Control.Lens.Setter.-~') is more flexible.
--
-- >>> (a,b) & _1 <<*~ c
-- (a,(a * c,b))
--
-- >>> (a,b) & _2 <<*~ c
-- (b,(a,b * c))
--
-- @
-- ('<<*~') :: 'Num' a => 'Lens'' s a -> a -> s -> (a, s)
-- ('<<*~') :: 'Num' a => 'Iso'' s a -> a -> s -> (a, s)
-- @
(<<*~) :: Num a => LensLike' ((,) a) s a -> a -> s -> (a, s)
l <<*~ b = l $ \a -> (a, a * b)
{-# INLINE (<<*~) #-}

-- | Divide the target of a numerically valued 'Lens' and return the old value.
--
-- When you do not need the old value, ('Control.Lens.Setter.//~') is more flexible.
--
-- >>> (a,b) & _1 <<//~ c
-- (a,(a / c,b))
--
-- >>> ("Hawaii",10) & _2 <<//~ 2
-- (10.0,("Hawaii",5.0))
--
-- @
-- ('<<//~') :: Fractional a => 'Lens'' s a -> a -> s -> (a, s)
-- ('<<//~') :: Fractional a => 'Iso'' s a -> a -> s -> (a, s)
-- @
(<<//~) :: Fractional a => LensLike' ((,) a) s a -> a -> s -> (a, s)
l <<//~ b = l $ \a -> (a, a / b)
{-# INLINE (<<//~) #-}

-- | Raise the target of a numerically valued 'Lens' to a non-negative power and return the old value.
--
-- When you do not need the old value, ('Control.Lens.Setter.^~') is more flexible.
--
-- @
-- ('<<^~') :: ('Num' a, 'Integral' e) => 'Lens'' s a -> e -> s -> (a, s)
-- ('<<^~') :: ('Num' a, 'Integral' e) => 'Iso'' s a -> e -> s -> (a, s)
-- @
(<<^~) :: (Num a, Integral e) => LensLike' ((,) a) s a -> e -> s -> (a, s)
l <<^~ e = l $ \a -> (a, a ^ e)
{-# INLINE (<<^~) #-}

-- | Raise the target of a fractionally valued 'Lens' to an integral power and return the old value.
--
-- When you do not need the old value, ('Control.Lens.Setter.^^~') is more flexible.
--
-- @
-- ('<<^^~') :: ('Fractional' a, 'Integral' e) => 'Lens'' s a -> e -> s -> (a, s)
-- ('<<^^~') :: ('Fractional' a, 'Integral' e) => 'Iso'' s a -> e -> S -> (a, s)
-- @
(<<^^~) :: (Fractional a, Integral e) => LensLike' ((,) a) s a -> e -> s -> (a, s)
l <<^^~ e = l $ \a -> (a, a ^^ e)
{-# INLINE (<<^^~) #-}

-- | Raise the target of a floating-point valued 'Lens' to an arbitrary power and return the old value.
--
-- When you do not need the old value, ('Control.Lens.Setter.**~') is more flexible.
--
-- >>> (a,b) & _1 <<**~ c
-- (a,(a**c,b))
--
-- >>> (a,b) & _2 <<**~ c
-- (b,(a,b**c))
--
-- @
-- ('<<**~') :: 'Floating' a => 'Lens'' s a -> a -> s -> (a, s)
-- ('<<**~') :: 'Floating' a => 'Iso'' s a -> a -> s -> (a, s)
-- @
(<<**~) :: Floating a => LensLike' ((,) a) s a -> a -> s -> (a, s)
l <<**~ e = l $ \a -> (a, a ** e)
{-# INLINE (<<**~) #-}

-- | Logically '||' the target of a 'Bool'-valued 'Lens' and return the old value.
--
-- When you do not need the old value, ('Control.Lens.Setter.||~') is more flexible.
--
-- >>> (False,6) & _1 <<||~ True
-- (False,(True,6))
--
-- >>> ("hello",True) & _2 <<||~ False
-- (True,("hello",True))
--
-- @
-- ('<<||~') :: 'Lens'' s 'Bool' -> 'Bool' -> s -> ('Bool', s)
-- ('<<||~') :: 'Iso'' s 'Bool' -> 'Bool' -> s -> ('Bool', s)
-- @
(<<||~) :: LensLike' ((,) Bool) s Bool -> Bool -> s -> (Bool, s)
l <<||~ b = l $ \a -> (a, b || a)
{-# INLINE (<<||~) #-}

-- | Logically '&&' the target of a 'Bool'-valued 'Lens' and return the old value.
--
-- When you do not need the old value, ('Control.Lens.Setter.&&~') is more flexible.
--
-- >>> (False,6) & _1 <<&&~ True
-- (False,(False,6))
--
-- >>> ("hello",True) & _2 <<&&~ False
-- (True,("hello",False))
--
-- @
-- ('<<&&~') :: 'Lens'' s Bool -> Bool -> s -> (Bool, s)
-- ('<<&&~') :: 'Iso'' s Bool -> Bool -> s -> (Bool, s)
-- @
(<<&&~) :: LensLike' ((,) Bool) s Bool -> Bool -> s -> (Bool, s)
l <<&&~ b = l $ \a -> (a, b && a)
{-# INLINE (<<&&~) #-}

-- | Modify the target of a monoidally valued 'Lens' by using ('<>') a new value and return the old value.
--
-- When you do not need the old value, ('Control.Lens.Setter.<>~') is more flexible.
--
-- >>> (Sum a,b) & _1 <<<>~ Sum c
-- (Sum {getSum = a},(Sum {getSum = a + c},b))
--
-- >>> _2 <<<>~ ", 007" $ ("James", "Bond")
-- ("Bond",("James","Bond, 007"))
--
-- @
-- ('<<<>~') :: 'Semigroup' r => 'Lens'' s r -> r -> s -> (r, s)
-- ('<<<>~') :: 'Semigroup' r => 'Iso'' s r -> r -> s -> (r, s)
-- @
(<<<>~) :: Semigroup r => LensLike' ((,) r) s r -> r -> s -> (r, s)
l <<<>~ b = l $ \a -> (a, a <> b)
{-# INLINE (<<<>~) #-}

-------------------------------------------------------------------------------
-- Setting and Remembering State
-------------------------------------------------------------------------------

-- | Modify the target of a 'Lens' into your 'Monad''s state by a user supplied
-- function and return the result.
--
-- When applied to a 'Control.Lens.Traversal.Traversal', it this will return a monoidal summary of all of the intermediate
-- results.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.%=') is more flexible.
--
-- @
-- ('<%=') :: 'MonadState' s m             => 'Lens'' s a      -> (a -> a) -> m a
-- ('<%=') :: 'MonadState' s m             => 'Control.Lens.Iso.Iso'' s a       -> (a -> a) -> m a
-- ('<%=') :: ('MonadState' s m, 'Monoid' a) => 'Control.Lens.Traversal.Traversal'' s a -> (a -> a) -> m a
-- @
(<%=) :: MonadState s m => LensLike ((,)b) s s a b -> (a -> b) -> m b
l <%= f = l %%= (\b -> (b, b)) . f
{-# INLINE (<%=) #-}


-- | Add to the target of a numerically valued 'Lens' into your 'Monad''s state
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

-- | Subtract from the target of a numerically valued 'Lens' into your 'Monad''s
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

-- | Multiply the target of a numerically valued 'Lens' into your 'Monad''s
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

-- | Divide the target of a fractionally valued 'Lens' into your 'Monad''s state
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

-- | Raise the target of a numerically valued 'Lens' into your 'Monad''s state
-- to a non-negative 'Integral' power and return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.^=') is more flexible.
--
-- @
-- ('<^=') :: ('MonadState' s m, 'Num' a, 'Integral' e) => 'Lens'' s a -> e -> m a
-- ('<^=') :: ('MonadState' s m, 'Num' a, 'Integral' e) => 'Control.Lens.Iso.Iso'' s a -> e -> m a
-- @
(<^=) :: (MonadState s m, Num a, Integral e) => LensLike' ((,)a) s a -> e -> m a
l <^= e = l <%= (^ e)
{-# INLINE (<^=) #-}

-- | Raise the target of a fractionally valued 'Lens' into your 'Monad''s state
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

-- | Raise the target of a floating-point valued 'Lens' into your 'Monad''s
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

-- | Logically '||' a Boolean valued 'Lens' into your 'Monad''s state and return
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

-- | Logically '&&' a Boolean valued 'Lens' into your 'Monad''s state and return
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

-- | Modify the target of a 'Lens' into your 'Monad''s state by a user supplied
-- function and return the /old/ value that was replaced.
--
-- When applied to a 'Control.Lens.Traversal.Traversal', this will return a monoidal summary of all of the old values
-- present.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.%=') is more flexible.
--
-- @
-- ('<<%=') :: 'MonadState' s m             => 'Lens'' s a      -> (a -> a) -> m a
-- ('<<%=') :: 'MonadState' s m             => 'Control.Lens.Iso.Iso'' s a       -> (a -> a) -> m a
-- ('<<%=') :: ('MonadState' s m, 'Monoid' a) => 'Control.Lens.Traversal.Traversal'' s a -> (a -> a) -> m a
-- @
--
-- @('<<%=') :: 'MonadState' s m => 'LensLike' ((,)a) s s a b -> (a -> b) -> m a@
(<<%=) :: (Strong p, MonadState s m) => Over p ((,)a) s s a b -> p a b -> m a
l <<%= f = l %%= lmap (\a -> (a,a)) (second' f)
{-# INLINE (<<%=) #-}

-- | Replace the target of a 'Lens' into your 'Monad''s state with a user supplied
-- value and return the /old/ value that was replaced.
--
-- When applied to a 'Control.Lens.Traversal.Traversal', this will return a monoidal summary of all of the old values
-- present.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter..=') is more flexible.
--
-- @
-- ('<<.=') :: 'MonadState' s m             => 'Lens'' s a      -> a -> m a
-- ('<<.=') :: 'MonadState' s m             => 'Control.Lens.Iso.Iso'' s a       -> a -> m a
-- ('<<.=') :: ('MonadState' s m, 'Monoid' a) => 'Control.Lens.Traversal.Traversal'' s a -> a -> m a
-- @
(<<.=) :: MonadState s m => LensLike ((,)a) s s a b -> b -> m a
l <<.= b = l %%= \a -> (a,b)
{-# INLINE (<<.=) #-}

-- | Replace the target of a 'Lens' into your 'Monad''s state with 'Just' a user supplied
-- value and return the /old/ value that was replaced.
--
-- When applied to a 'Control.Lens.Traversal.Traversal', this will return a monoidal summary of all of the old values
-- present.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.?=') is more flexible.
--
-- @
-- ('<<?=') :: 'MonadState' s m             => 'Lens' s t a (Maybe b)      -> b -> m a
-- ('<<?=') :: 'MonadState' s m             => 'Control.Lens.Iso.Iso' s t a (Maybe b)       -> b -> m a
-- ('<<?=') :: ('MonadState' s m, 'Monoid' a) => 'Control.Lens.Traversal.Traversal' s t a (Maybe b) -> b -> m a
-- @
(<<?=) :: MonadState s m => LensLike ((,)a) s s a (Maybe b) -> b -> m a
l <<?= b = l <<.= Just b
{-# INLINE (<<?=) #-}

-- | Modify the target of a 'Lens' into your 'Monad''s state by adding a value
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.+=') is more flexible.
--
-- @
-- ('<<+=') :: ('MonadState' s m, 'Num' a) => 'Lens'' s a -> a -> m a
-- ('<<+=') :: ('MonadState' s m, 'Num' a) => 'Iso'' s a -> a -> m a
-- @
(<<+=) :: (MonadState s m, Num a) => LensLike' ((,) a) s a -> a -> m a
l <<+= n = l %%= \a -> (a, a + n)
{-# INLINE (<<+=) #-}

-- | Modify the target of a 'Lens' into your 'Monad''s state by subtracting a value
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.-=') is more flexible.
--
-- @
-- ('<<-=') :: ('MonadState' s m, 'Num' a) => 'Lens'' s a -> a -> m a
-- ('<<-=') :: ('MonadState' s m, 'Num' a) => 'Iso'' s a -> a -> m a
-- @
(<<-=) :: (MonadState s m, Num a) => LensLike' ((,) a) s a -> a -> m a
l <<-= n = l %%= \a -> (a, a - n)
{-# INLINE (<<-=) #-}

-- | Modify the target of a 'Lens' into your 'Monad''s state by multipling a value
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.*=') is more flexible.
--
-- @
-- ('<<*=') :: ('MonadState' s m, 'Num' a) => 'Lens'' s a -> a -> m a
-- ('<<*=') :: ('MonadState' s m, 'Num' a) => 'Iso'' s a -> a -> m a
-- @
(<<*=) :: (MonadState s m, Num a) => LensLike' ((,) a) s a -> a -> m a
l <<*= n = l %%= \a -> (a, a * n)
{-# INLINE (<<*=) #-}

-- | Modify the target of a 'Lens' into your 'Monad'\s state by dividing by a value
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.//=') is more flexible.
--
-- @
-- ('<<//=') :: ('MonadState' s m, 'Fractional' a) => 'Lens'' s a -> a -> m a
-- ('<<//=') :: ('MonadState' s m, 'Fractional' a) => 'Iso'' s a -> a -> m a
-- @
(<<//=) :: (MonadState s m, Fractional a) => LensLike' ((,) a) s a -> a -> m a
l <<//= n = l %%= \a -> (a, a / n)
{-# INLINE (<<//=) #-}

-- | Modify the target of a 'Lens' into your 'Monad''s state by raising it by a non-negative power
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.^=') is more flexible.
--
-- @
-- ('<<^=') :: ('MonadState' s m, 'Num' a, 'Integral' e) => 'Lens'' s a -> e -> m a
-- ('<<^=') :: ('MonadState' s m, 'Num' a, 'Integral' e) => 'Iso'' s a -> a -> m a
-- @
(<<^=) :: (MonadState s m, Num a, Integral e) => LensLike' ((,) a) s a -> e -> m a
l <<^= n = l %%= \a -> (a, a ^ n)
{-# INLINE (<<^=) #-}

-- | Modify the target of a 'Lens' into your 'Monad''s state by raising it by an integral power
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.^^=') is more flexible.
--
-- @
-- ('<<^^=') :: ('MonadState' s m, 'Fractional' a, 'Integral' e) => 'Lens'' s a -> e -> m a
-- ('<<^^=') :: ('MonadState' s m, 'Fractional' a, 'Integral' e) => 'Iso'' s a -> e -> m a
-- @
(<<^^=) :: (MonadState s m, Fractional a, Integral e) => LensLike' ((,) a) s a -> e -> m a
l <<^^= n = l %%= \a -> (a, a ^^ n)
{-# INLINE (<<^^=) #-}

-- | Modify the target of a 'Lens' into your 'Monad''s state by raising it by an arbitrary power
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.**=') is more flexible.
--
-- @
-- ('<<**=') :: ('MonadState' s m, 'Floating' a) => 'Lens'' s a -> a -> m a
-- ('<<**=') :: ('MonadState' s m, 'Floating' a) => 'Iso'' s a -> a -> m a
-- @
(<<**=) :: (MonadState s m, Floating a) => LensLike' ((,) a) s a -> a -> m a
l <<**= n = l %%= \a -> (a, a ** n)
{-# INLINE (<<**=) #-}

-- | Modify the target of a 'Lens' into your 'Monad''s state by taking its logical '||' with a value
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.||=') is more flexible.
--
-- @
-- ('<<||=') :: 'MonadState' s m => 'Lens'' s 'Bool' -> 'Bool' -> m 'Bool'
-- ('<<||=') :: 'MonadState' s m => 'Iso'' s 'Bool' -> 'Bool' -> m 'Bool'
-- @
(<<||=) :: MonadState s m => LensLike' ((,) Bool) s Bool -> Bool -> m Bool
l <<||= b = l %%= \a -> (a, a || b)
{-# INLINE (<<||=) #-}

-- | Modify the target of a 'Lens' into your 'Monad''s state by taking its logical '&&' with a value
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.&&=') is more flexible.
--
-- @
-- ('<<&&=') :: 'MonadState' s m => 'Lens'' s 'Bool' -> 'Bool' -> m 'Bool'
-- ('<<&&=') :: 'MonadState' s m => 'Iso'' s 'Bool' -> 'Bool' -> m 'Bool'
-- @
(<<&&=) :: MonadState s m => LensLike' ((,) Bool) s Bool -> Bool -> m Bool
l <<&&= b = l %%= \a -> (a, a && b)
{-# INLINE (<<&&=) #-}

-- | Modify the target of a 'Lens' into your 'Monad''s state by using ('<>')
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.<>=') is more flexible.
--
-- @
-- ('<<<>=') :: ('MonadState' s m, 'Semigroup' r) => 'Lens'' s r -> r -> m r
-- ('<<<>=') :: ('MonadState' s m, 'Semigroup' r) => 'Iso'' s r -> r -> m r
-- @
(<<<>=) :: (MonadState s m, Semigroup r) => LensLike' ((,) r) s r -> r -> m r
l <<<>= b = l %%= \a -> (a, a <> b)
{-# INLINE (<<<>=) #-}

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
  modify $ \s -> ipeek b (l sell s)
  return b
{-# INLINE (<<~) #-}

-- | ('<>') a 'Semigroup' value onto the end of the target of a 'Lens' and
-- return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.<>~') is more flexible.
(<<>~) :: Semigroup m => LensLike ((,)m) s t m m -> m -> s -> (m, t)
l <<>~ m = l <%~ (<> m)
{-# INLINE (<<>~) #-}

-- | ('<>') a 'Semigroup' value onto the end of the target of a 'Lens' into
-- your 'Monad''s state and return the result.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.<>=') is more flexible.
(<<>=) :: (MonadState s m, Semigroup r) => LensLike' ((,)r) s r -> r -> m r
l <<>= r = l <%= (<> r)
{-# INLINE (<<>=) #-}

-- | ('<>') a 'Semigroup' value onto the end of the target of a 'Lens' and
-- return the result.
-- However, unlike '<<>~', it is prepend to the head side.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.<>:~') is more flexible.
(<<>:~) :: Semigroup m => LensLike ((,)m) s t m m -> m -> s -> (m, t)
l <<>:~ m = l <%~ (m <>)
{-# INLINE (<<>:~) #-}

-- | ('<>') a 'Semigroup' value onto the end of the target of a 'Lens' into
-- your 'Monad''s state and return the result.
-- However, unlike '<<>=', it is prepend to the head side.
--
-- When you do not need the result of the operation, ('Control.Lens.Setter.<>:=') is more flexible.
(<<>:=) :: (MonadState s m, Semigroup r) => LensLike' ((,)r) s r -> r -> m r
l <<>:= r = l <%= (r <>)
{-# INLINE (<<>:=) #-}

------------------------------------------------------------------------------
-- Arrow operators
------------------------------------------------------------------------------

-- | 'Control.Lens.Setter.over' for Arrows.
--
-- Unlike 'Control.Lens.Setter.over', 'overA' can't accept a simple
-- 'Control.Lens.Setter.Setter', but requires a full lens, or close
-- enough.
--
-- >>> overA _1 ((+1) *** (+2)) ((1,2),6)
-- ((2,4),6)
--
-- @
-- overA :: Arrow ar => Lens s t a b -> ar a b -> ar s t
-- @
overA :: Arrow ar => LensLike (Context a b) s t a b -> ar a b -> ar s t
overA l p = arr (\s -> let (Context f a) = l sell s in (f, a))
            >>> second p
            >>> arr (uncurry id)

------------------------------------------------------------------------------
-- Indexed
------------------------------------------------------------------------------

-- | Adjust the target of an 'IndexedLens' returning the intermediate result, or
-- adjust all of the targets of an 'Control.Lens.Traversal.IndexedTraversal' and return a monoidal summary
-- along with the answer.
--
-- @
-- l '<%~' f ≡ l '<%@~' 'const' f
-- @
--
-- When you do not need access to the index then ('<%~') is more liberal in what it can accept.
--
-- If you do not need the intermediate result, you can use ('Control.Lens.Setter.%@~') or even ('Control.Lens.Setter.%~').
--
-- @
-- ('<%@~') ::             'IndexedLens' i s t a b      -> (i -> a -> b) -> s -> (b, t)
-- ('<%@~') :: 'Monoid' b => 'Control.Lens.Traversal.IndexedTraversal' i s t a b -> (i -> a -> b) -> s -> (b, t)
-- @
(<%@~) :: Over (Indexed i) ((,) b) s t a b -> (i -> a -> b) -> s -> (b, t)
l <%@~ f = l (Indexed $ \i a -> let b = f i a in (b, b))
{-# INLINE (<%@~) #-}

-- | Adjust the target of an 'IndexedLens' returning the old value, or
-- adjust all of the targets of an 'Control.Lens.Traversal.IndexedTraversal' and return a monoidal summary
-- of the old values along with the answer.
--
-- @
-- ('<<%@~') ::             'IndexedLens' i s t a b      -> (i -> a -> b) -> s -> (a, t)
-- ('<<%@~') :: 'Monoid' a => 'Control.Lens.Traversal.IndexedTraversal' i s t a b -> (i -> a -> b) -> s -> (a, t)
-- @
(<<%@~) :: Over (Indexed i) ((,) a) s t a b -> (i -> a -> b) -> s -> (a, t)
l <<%@~ f = l $ Indexed $ \i a -> second' (f i) (a,a)

{-# INLINE (<<%@~) #-}

-- | Adjust the target of an 'IndexedLens' returning a supplementary result, or
-- adjust all of the targets of an 'Control.Lens.Traversal.IndexedTraversal' and return a monoidal summary
-- of the supplementary results and the answer.
--
-- @
-- ('%%@~') ≡ 'Control.Lens.Indexed.withIndex'
-- @
--
-- @
-- ('%%@~') :: 'Functor' f => 'IndexedLens' i s t a b      -> (i -> a -> f b) -> s -> f t
-- ('%%@~') :: 'Applicative' f => 'Control.Lens.Traversal.IndexedTraversal' i s t a b -> (i -> a -> f b) -> s -> f t
-- @
--
-- In particular, it is often useful to think of this function as having one of these even more
-- restricted type signatures:
--
-- @
-- ('%%@~') ::             'IndexedLens' i s t a b      -> (i -> a -> (r, b)) -> s -> (r, t)
-- ('%%@~') :: 'Monoid' r => 'Control.Lens.Traversal.IndexedTraversal' i s t a b -> (i -> a -> (r, b)) -> s -> (r, t)
-- @
(%%@~) :: Over (Indexed i) f s t a b -> (i -> a -> f b) -> s -> f t
(%%@~) l = l .# Indexed
{-# INLINE (%%@~) #-}

-- | Adjust the target of an 'IndexedLens' returning a supplementary result, or
-- adjust all of the targets of an 'Control.Lens.Traversal.IndexedTraversal' within the current state, and
-- return a monoidal summary of the supplementary results.
--
-- @
-- l '%%@=' f ≡ 'state' (l '%%@~' f)
-- @
--
-- @
-- ('%%@=') :: 'MonadState' s m                 => 'IndexedLens' i s s a b      -> (i -> a -> (r, b)) -> s -> m r
-- ('%%@=') :: ('MonadState' s m, 'Monoid' r) => 'Control.Lens.Traversal.IndexedTraversal' i s s a b -> (i -> a -> (r, b)) -> s -> m r
-- @
(%%@=) :: MonadState s m => Over (Indexed i) ((,) r) s s a b -> (i -> a -> (r, b)) -> m r
l %%@= f = State.state (l %%@~ f)
{-# INLINE (%%@=) #-}

-- | Adjust the target of an 'IndexedLens' returning the intermediate result, or
-- adjust all of the targets of an 'Control.Lens.Traversal.IndexedTraversal' within the current state, and
-- return a monoidal summary of the intermediate results.
--
-- @
-- ('<%@=') :: 'MonadState' s m                 => 'IndexedLens' i s s a b      -> (i -> a -> b) -> m b
-- ('<%@=') :: ('MonadState' s m, 'Monoid' b) => 'Control.Lens.Traversal.IndexedTraversal' i s s a b -> (i -> a -> b) -> m b
-- @
(<%@=) :: MonadState s m => Over (Indexed i) ((,) b) s s a b -> (i -> a -> b) -> m b
l <%@= f = l %%@= \ i a -> let b = f i a in (b, b)
{-# INLINE (<%@=) #-}

-- | Adjust the target of an 'IndexedLens' returning the old value, or
-- adjust all of the targets of an 'Control.Lens.Traversal.IndexedTraversal' within the current state, and
-- return a monoidal summary of the old values.
--
-- @
-- ('<<%@=') :: 'MonadState' s m                 => 'IndexedLens' i s s a b      -> (i -> a -> b) -> m a
-- ('<<%@=') :: ('MonadState' s m, 'Monoid' b) => 'Control.Lens.Traversal.IndexedTraversal' i s s a b -> (i -> a -> b) -> m a
-- @
(<<%@=) :: MonadState s m => Over (Indexed i) ((,) a) s s a b -> (i -> a -> b) -> m a
l <<%@= f = State.state (l (Indexed $ \ i a -> (a, f i a)))
{-# INLINE (<<%@=) #-}

------------------------------------------------------------------------------
-- ALens Combinators
------------------------------------------------------------------------------

-- | A version of ('Control.Lens.Getter.^.') that works on 'ALens'.
--
-- >>> ("hello","world")^#_2
-- "world"
(^#) :: s -> ALens s t a b -> a
s ^# l = ipos (l sell s)
{-# INLINE (^#) #-}

-- | A version of 'Control.Lens.Setter.set' that works on 'ALens'.
--
-- >>> storing _2 "world" ("hello","there")
-- ("hello","world")
storing :: ALens s t a b -> b -> s -> t
storing l b s = ipeek b (l sell s)
{-# INLINE storing #-}

-- | A version of ('Control.Lens.Setter..~') that works on 'ALens'.
--
-- >>> ("hello","there") & _2 #~ "world"
-- ("hello","world")
(#~) :: ALens s t a b -> b -> s -> t
(#~) l b s = ipeek b (l sell s)
{-# INLINE (#~) #-}

-- | A version of ('Control.Lens.Setter.%~') that works on 'ALens'.
--
-- >>> ("hello","world") & _2 #%~ length
-- ("hello",5)
(#%~) :: ALens s t a b -> (a -> b) -> s -> t
(#%~) l f s = ipeeks f (l sell s)
{-# INLINE (#%~) #-}

-- | A version of ('%%~') that works on 'ALens'.
--
-- >>> ("hello","world") & _2 #%%~ \x -> (length x, x ++ "!")
-- (5,("hello","world!"))
(#%%~) :: Functor f => ALens s t a b -> (a -> f b) -> s -> f t
(#%%~) l f s = runPretext (l sell s) f
{-# INLINE (#%%~) #-}

-- | A version of ('Control.Lens.Setter..=') that works on 'ALens'.
(#=) :: MonadState s m => ALens s s a b -> b -> m ()
l #= f = modify (l #~ f)
{-# INLINE (#=) #-}

-- | A version of ('Control.Lens.Setter.%=') that works on 'ALens'.
(#%=) :: MonadState s m => ALens s s a b -> (a -> b) -> m ()
l #%= f = modify (l #%~ f)
{-# INLINE (#%=) #-}

-- | A version of ('<%~') that works on 'ALens'.
--
-- >>> ("hello","world") & _2 <#%~ length
-- (5,("hello",5))
(<#%~) :: ALens s t a b -> (a -> b) -> s -> (b, t)
l <#%~ f = \s -> runPretext (l sell s) $ \a -> let b = f a in (b, b)
{-# INLINE (<#%~) #-}

-- | A version of ('<%=') that works on 'ALens'.
(<#%=) :: MonadState s m => ALens s s a b -> (a -> b) -> m b
l <#%= f = l #%%= \a -> let b = f a in (b, b)
{-# INLINE (<#%=) #-}

-- | A version of ('%%=') that works on 'ALens'.
(#%%=) :: MonadState s m => ALens s s a b -> (a -> (r, b)) -> m r
l #%%= f = State.state $ \s -> runPretext (l sell s) f
{-# INLINE (#%%=) #-}

-- | A version of ('Control.Lens.Setter.<.~') that works on 'ALens'.
--
-- >>> ("hello","there") & _2 <#~ "world"
-- ("world",("hello","world"))
(<#~) :: ALens s t a b -> b -> s -> (b, t)
l <#~ b = \s -> (b, storing l b s)
{-# INLINE (<#~) #-}

-- | A version of ('Control.Lens.Setter.<.=') that works on 'ALens'.
(<#=) :: MonadState s m => ALens s s a b -> b -> m b
l <#= b = do
  l #= b
  return b
{-# INLINE (<#=) #-}

-- | There is a field for every type in the 'Void'. Very zen.
--
-- >>> [] & mapped.devoid +~ 1
-- []
--
-- >>> Nothing & mapped.devoid %~ abs
-- Nothing
--
-- @
-- 'devoid' :: 'Lens'' 'Void' a
-- @
devoid :: Over p f Void Void a b
devoid _ = absurd
{-# INLINE devoid #-}

-- | We can always retrieve a @()@ from any type.
--
-- >>> "hello"^.united
-- ()
--
-- >>> "hello" & united .~ ()
-- "hello"
united :: Lens' a ()
united f v = f () <&> \ () -> v
{-# INLINE united #-}

data First1 f a = First1 (f a) a

instance (Functor f) => Functor (First1 f) where
  fmap f (First1 fa a) = First1 (f <$> fa) (f a)
  {-# INLINE fmap #-}

instance (Functor f) => Apply (First1 f) where
  First1 ff f <.> First1 _ x = First1 (($ x) <$> ff) (f x)
  {-# INLINE (<.>) #-}

getFirst1 :: First1 f a -> f a
getFirst1 (First1 fa _) = fa
{-# INLINE getFirst1 #-}

-- | A 'Lens' focusing on the first element of a 'Traversable1' container.
--
-- >>> 2 :| [3, 4] & head1 +~ 10
-- 12 :| [3,4]
--
-- >>> Identity True ^. head1
-- True
head1 :: (Traversable1 t) => Lens' (t a) a
head1 f = getFirst1 . traverse1 (\a -> First1 (f a) a)
{-# INLINE head1 #-}

-- | A 'Lens' focusing on the last element of a 'Traversable1' container.
--
-- >>> 2 :| [3, 4] & last1 +~ 10
-- 2 :| [3,14]
--
-- >>> Node 'a' [Node 'b' [], Node 'c' []] ^. last1
-- 'c'
last1 :: (Traversable1 t) => Lens' (t a) a
last1 f = fmap getReverse . head1 f . Reverse
{-# INLINE last1 #-}

-- | Fuse a composition of lenses using 'Yoneda' to provide 'fmap' fusion.
--
-- In general, given a pair of lenses 'foo' and 'bar'
--
-- @
-- fusing (foo.bar) = foo.bar
-- @
--
-- however, @foo@ and @bar@ are either going to 'fmap' internally or they are trivial.
--
-- 'fusing' exploits the 'Yoneda' lemma to merge these separate uses into a single 'fmap'.
--
-- This is particularly effective when the choice of functor 'f' is unknown at compile
-- time or when the 'Lens' @foo.bar@ in the above description is recursive or complex
-- enough to prevent inlining.
--
-- @
-- 'fusing' :: 'Lens' s t a b -> 'Lens' s t a b
-- @
fusing :: Functor f => LensLike (Yoneda f) s t a b -> LensLike f s t a b
fusing t = \f -> lowerYoneda . t (liftYoneda . f)
{-# INLINE fusing #-}

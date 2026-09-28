{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Unsound
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- One commonly asked question is: can we combine two lenses,
-- @'Lens'' a b@ and @'Lens'' a c@ into @'Lens'' a (b, c)@.
-- This is fair thing to ask, but such operation is unsound in general.
-- See `lensProduct`.
--
-------------------------------------------------------------------------------
module Control.Lens.Unsound
  (
    lensProduct
  , prismSum
  , adjoin
  , setterUnion
  ) where

import Control.Lens
import Control.Lens.Internal.Prelude
import Prelude ()

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens

-- | A lens product. There is no law-abiding way to do this in general.
-- Result is only a valid t'Lens' if the input lenses project disjoint parts of
-- the structure @s@. Otherwise "you get what you put in" law
--
-- @
-- 'Control.Lens.Getter.view' l ('Control.Lens.Setter.set' l v s) ≡ v
-- @
--
-- is violated by
--
-- >>> let badLens :: Lens' (Int, Char) (Int, Int); badLens = lensProduct _1 _1
-- >>> view badLens (set badLens (1,2) (3,'x'))
-- (2,2)
--
-- but we should get @(1,2)@.
--
-- Are you looking for 'Control.Lens.Lens.alongside'?
--
lensProduct :: ALens' s a -> ALens' s b -> Lens' s (a, b)
lensProduct l1 l2 f s =
    f (s ^# l1, s ^# l2) <&> \(a, b) -> s & l1 #~ a & l2 #~ b

-- | A dual of `lensProduct`: a prism sum.
--
-- The law
--
-- @
-- 'Control.Lens.Fold.preview' l ('Control.Lens.Review.review' l b) ≡ 'Just' b
-- @
--
-- breaks with
--
-- >>> let badPrism :: Prism' (Maybe Char) (Either Char Char); badPrism = prismSum _Just _Just
-- >>> preview badPrism (review badPrism (Right 'x'))
-- Just (Left 'x')
--
-- We put in 'Right' value, but get back 'Left'.
--
-- Are you looking for 'Control.Lens.Prism.without'?
--
prismSum :: APrism s t a b
         -> APrism s t c d
         -> Prism s t (Either a c) (Either b d)
prismSum k k' =
    withPrism k                  $ \bt seta ->
    withPrism k'                 $ \dt setb ->
    prism (either bt dt) $ \s ->
    f (Left <$> seta s) (Right <$> setb s)
  where
    f a@(Right _) _ = a
    f (Left _)    b = b

-- | A generalization of `mappend`ing folds: A union of disjoint traversals.
--
-- Traversing the same entry twice is illegal.
--
-- Are you looking for 'Control.Lens.Traversal.failing'?
--
adjoin :: Traversal' s a -> Traversal' s a -> Traversal' s a
adjoin t1 t2 =
    lensProduct (partsOf t1) (partsOf t2) . both . each

-- | A union of setters: apply the same function through both of them.
--
-- Unlike `adjoin`, this needs only 'ASetter's, so it reaches targets that
-- cannot be traversed. In exchange the result is write-only: unlike `adjoin`
-- there is nothing to read back through.
--
-- Result is only a valid t'Setter' if the input setters touch disjoint parts of
-- the structure. Otherwise the composition law
--
-- @
-- 'Control.Lens.Setter.over' l f '.' 'Control.Lens.Setter.over' l g ≡ 'Control.Lens.Setter.over' l (f '.' g)
-- @
--
-- is violated, because @f@ is applied once per setter on the left but the
-- composite is applied once per setter on the right:
--
-- >>> let badSetter :: Setter' Int Int; badSetter = setterUnion id id
-- >>> over badSetter (+1) (over badSetter (*2) 1)
-- 6
--
-- >>> over badSetter ((+1) . (*2)) 1
-- 7
--
-- On disjoint setters it behaves as expected:
--
-- >>> let (f, _, g) = over (setterUnion (_1 . mapped) (_3 . mapped)) (*2) ((+1), 'x', (+10))
-- >>> (f 1, g 1)
-- (4,22)
--
-- The second setter runs first, and the structure it produces is what the first
-- setter consumes, so the two may change the type in sequence:
--
-- >>> over (setterUnion _2 _1) show (1 :: Int, 2 :: Int)
-- ("1","2")
--
-- @
-- 'setterUnion' :: 'Setter'' s a -> 'Setter'' s a -> 'Setter'' s a
-- @
--
-- Are you looking for 'Control.Lens.Setter.bimapped' or
-- 'Control.Lens.Traversal.both'?
--
setterUnion :: ASetter x o a b -> ASetter i x a b -> IndexPreservingSetter i o a b
setterUnion l1 l2 = setting (\f -> over l1 f . over l2 f)
{-# INLINE setterUnion #-}

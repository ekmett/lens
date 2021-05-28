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
-- @`Lens'` a b@ and @`Lens'` a c@ into @`Lens'` a (b, c)@.
-- This is fair thing to ask, but such operation is unsound in general.
-- See `lensProduct`.
--
-------------------------------------------------------------------------------
module Control.Lens.Unsound
  (
    lensProduct
  , prismSum
  , adjoin
  ) where

import Control.Lens
import Control.Lens.Internal.Prelude
import Prelude ()

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens

-- | A lens product. There is no law-abiding way to do this in general.
-- Result is only a valid 'Lens' if the input lenses project disjoint parts of
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

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Magma
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Magma
  ( Magma(..)
  , size
  , magmaIns
  , magmaOuts
  , magma
  , nullLeft
  , nullRight
  , maximal
  ) where

import Control.Applicative
import Control.Lens.Indexed
import Control.Lens.Internal
import Control.Lens.Lens
import Control.Lens.Traversal
import Data.Monoid
import Data.Foldable
import Data.Profunctor.Unsafe

------------------------------------------------------------------------------
-- Magma
------------------------------------------------------------------------------

data Magma i a
  = Ap Int         -- size
       Bool        -- left-to-right null check
       Bool        -- right-to-left null check
       (Last i)
       (Magma i a) -- left
       (Magma i a) -- right
  | Leaf i a
  | Pure
  deriving Show

size :: Magma i a -> Int
size (Ap s _ _ _ _ _) = s
size Leaf{}           = 1
size Pure             = 0
{-# INLINE size #-}

nullLeft :: Magma i a -> Bool
nullLeft (Ap _ nl _ _ _ _) = nl
nullLeft (Leaf _ _)        = False
nullLeft Pure              = True
{-# INLINE nullLeft #-}

nullRight :: Magma i a -> Bool
nullRight (Ap _ _ nr _ _ _) = nr
nullRight (Leaf _ _)        = False
nullRight Pure              = True
{-# INLINE nullRight #-}

maximal :: Magma i a -> Last i
maximal (Ap _ _ _ li _ _) = li
maximal (Leaf i _)        = Last (Just i)
maximal Pure              = Last Nothing
{-# INLINE maximal #-}

instance Functor (Magma i) where
  fmap f (Ap m nl nr li l r) = Ap m nl nr li (fmap f l) (fmap f r)
  fmap f (Leaf i a)          = Leaf i (f a)
  fmap _ Pure                = Pure
  {-# INLINE fmap #-}

instance Foldable (Magma i) where
  foldMap f (Ap _ _ _ _ l r) = foldMap f l `mappend` foldMap f r
  foldMap f (Leaf _ a)       = f a
  foldMap _ Pure             = mempty
  {-# INLINE foldMap #-}

instance Traversable (Magma i) where
  traverse f (Ap m nl nr li l r) = Ap m nl nr li <$> traverse f l <*> traverse f r
  traverse f (Leaf i a)          = Leaf i <$> f a
  traverse _ Pure                = pure Pure
  {-# INLINE traverse #-}

instance FunctorWithIndex i (Magma i) where
  imap f = go where
    go (Ap m nl nr li l r) = Ap m nl nr li (go l) (go r)
    go (Leaf i a)          = Leaf i (f i a)
    go Pure                = Pure
  {-# INLINE imap #-}

instance FoldableWithIndex i (Magma i) where
  ifoldMap f = go where
    go (Ap _ _ _ _ l r) = go l `mappend` go r
    go (Leaf i a)       = f i a
    go Pure             = mempty
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex i (Magma i) where
  itraverse f = go where
    go (Ap m nl nr li l r) = Ap m nl nr li <$> go l <*> go r
    go (Leaf i a)          = Leaf i <$> f i a
    go Pure                = pure Pure
  {-# INLINE itraverse #-}

-- | This is an illegal 'Monoid'.
instance Monoid (Magma i a) where
  mempty = Pure
  {-# INLINE mempty #-}

  mappend l r = Ap (size l + size r) (nullLeft l && nullLeft r) (nullRight r && nullRight l) (maximal l <> maximal r) l r
  {-# INLINE mappend #-}

magmaIns :: Bazaar (Indexed i) a b t -> Magma i a
magmaIns (Bazaar bz) = runAccessor $ bz $ Indexed (\i -> Accessor #. Leaf i)
{-# INLINE magmaIns #-}

------------------------------------------------------------------------------
-- Putting it back in the tree
------------------------------------------------------------------------------

newtype Flow i b a = Flow { runFlow :: Magma i b -> a }

instance Functor (Flow i b) where
  fmap f (Flow g) = Flow (f . g)
  {-# INLINE fmap #-}

-- | This is an illegal 'Applicative'.
instance Applicative (Flow i b) where
  pure a = Flow (const a)
  {-# INLINE pure #-}
  Flow mf <*> Flow ma = Flow $ \ s -> case s of
    Ap _ _ _ _ l r -> mf l (ma r)
    _              -> mf s (ma s)
  {-# INLINE (<*>) #-}

magmaOuts :: Bazaar (Indexed i) a b t -> Magma j b -> t
magmaOuts bz = runFlow $ runBazaar bz $ Indexed $ \ _ _ -> Flow $ \ t -> case t of
  Leaf _ a -> a
  _        -> error "magmaOuts: wrong shape"
{-# INLINE magmaOuts #-}

-- | This is only a valid 'Lens' if you don't change the shape of the 'Magma'.
magma :: AnIndexedTraversal i s t a b -> Lens s t (Magma i a) (Magma j b)
magma l f s = magmaOuts bz <$> f (magmaIns bz) where
  bz = l sell s
{-# INLINE magma #-}

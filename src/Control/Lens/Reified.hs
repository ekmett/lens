{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Reified
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
------------------------------------------------------------------------------
module Control.Lens.Reified where

import Control.Monad
import Control.Monad.Reader.Class
import Control.Applicative
import Control.Lens.Getter
import Control.Lens.Fold
import Control.Lens.Type
import Control.Lens.Traversal (ignored)

------------------------------------------------------------------------------
-- Reifying
------------------------------------------------------------------------------

-- | Reify a 'Lens' so it can be stored safely in a container.
newtype ReifiedLens s t a b = Lens { runLens :: Lens s t a b }

-- | @
-- type 'ReifiedLens'' = 'Simple' 'ReifiedLens'
-- @
type ReifiedLens' s a = ReifiedLens s s a a

-- | Reify an 'IndexedLens' so it can be stored safely in a container.
newtype ReifiedIndexedLens i s t a b = IndexedLens { runIndexedLens :: IndexedLens i s t a b }

-- | @
-- type 'ReifiedIndexedLens'' i = 'Simple' ('ReifiedIndexedLens' i)
-- @
type ReifiedIndexedLens' i s a = ReifiedIndexedLens i s s a a

-- | Reify an 'IndexedTraversal' so it can be stored safely in a container.
newtype ReifiedIndexedTraversal i s t a b = IndexedTraversal { runIndexedTraversal :: IndexedTraversal i s t a b }

-- | @
-- type 'ReifiedIndexedTraversal'' i = 'Simple' ('ReifiedIndexedTraversal' i)
-- @
type ReifiedIndexedTraversal' i s a = ReifiedIndexedTraversal i s s a a

-- | A form of 'Traversal' that can be stored monomorphically in a container.
newtype ReifiedTraversal s t a b = Traversal { runTraversal :: Traversal s t a b }

-- | @
-- type 'ReifiedTraversal'' = 'Simple' 'ReifiedTraversal'
-- @
type ReifiedTraversal' s a = ReifiedTraversal s s a a

-- | Reify a 'Getter' so it can be stored safely in a container.
newtype ReifiedGetter s a = Getter { runGetter :: Getter s a }

instance Functor (ReifiedGetter s) where
  fmap f (Getter l) = Getter (l.to f)

instance Applicative (ReifiedGetter s) where
  pure a = Getter $ to $ \_ -> a
  Getter mf <*> Getter ma = Getter $ to $ \s -> view mf s (view ma s)

instance Monad (ReifiedGetter s) where
  return a = Getter $ to $ \_ -> a
  Getter ma >>= f = Getter $ to $ \s -> view (runGetter (f (view ma s))) s

instance MonadReader s (ReifiedGetter s) where
  ask = Getter id
  local f (Getter m) = Getter (to f . m)

-- | Reify an 'IndexedGetter' so it can be stored safely in a container.
newtype ReifiedIndexedGetter i s a = IndexedGetter { runIndexedGetter :: IndexedGetter i s a }

-- | Reify a 'Fold' so it can be stored safely in a container.
newtype ReifiedFold s a = Fold { runFold :: Fold s a }

instance Functor (ReifiedFold s) where
  fmap f (Fold l) = Fold (l.to f)

instance Applicative (ReifiedFold s) where
  pure a = Fold $ folding $ \_ -> [a]
  Fold mf <*> Fold ma = Fold $ folding $ \s -> toListOf mf s <*> toListOf ma s

instance Alternative (ReifiedFold s) where
  empty = Fold ignored
  Fold ma <|> Fold mb = Fold $ folding (\s -> toListOf ma s ++ toListOf mb s)

instance Monad (ReifiedFold s) where
  return a = Fold $ folding $ \_ -> [a]
  Fold ma >>= f = Fold $ folding $ \s -> toListOf ma s >>= \a -> toListOf (runFold (f a)) s

instance MonadPlus (ReifiedFold s) where
  mzero = empty
  mplus = (<|>)

instance MonadReader s (ReifiedFold s) where
  ask = Fold $ folding (: [])
  local f (Fold m) = Fold (to f . m)

newtype ReifiedIndexedFold i s a = IndexedFold { runIndexedFold :: IndexedFold i s a }

-- | Reify a 'Setter' so it can be stored safely in a container.
newtype ReifiedSetter s t a b = Setter { runSetter :: Setter s t a b }

-- | @
-- type 'ReifiedSetter'' = 'Simple' 'ReifiedSetter'
-- @
type ReifiedSetter' s a = ReifiedSetter s s a a

-- | Reify an 'IndexedSetter' so it can be stored safely in a container.
newtype ReifiedIndexedSetter i s t a b =
  IndexedSetter { runIndexedSetter :: IndexedSetter i s t a b }

-- | @
-- type 'ReifiedIndexedSetter'' i = 'Simple' ('ReifiedIndexedSetter' i)
-- @
type ReifiedIndexedSetter' i s a = ReifiedIndexedSetter i s s a a

-- | Reify an 'Iso' so it can be stored safely in a container.
newtype ReifiedIso s t a b = Iso { runIso :: Iso s t a b }

-- | @
-- type 'ReifiedIso'' = 'Simple' 'ReifiedIso'
-- @
type ReifiedIso' s a = ReifiedIso s s a a

-- | Reify a 'Prism' so it can be stored safely in a container.
newtype ReifiedPrism s t a b = Prism { runPrism :: Prism s t a b }

-- | @
-- type 'ReifiedPrism'' = 'Simple' 'ReifiedPrism'
-- @
type ReifiedPrism' s a = ReifiedPrism s s a a

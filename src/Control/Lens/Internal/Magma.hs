{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Magma
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Magma
  ( Leaf(..)
  -- * Scoria
  , Scoria(..)
  , Molten(..)
  -- * Levels
  , Level(..)
  , Deepening(..), deepening
  , Flows(..)
  ) where

import Control.Applicative
import Control.Category
import Control.Comonad
import Control.Lens.Internal.Bazaar
import Control.Lens.Internal.Context
import Control.Lens.Internal.Indexed
import Data.Foldable
import Data.Int
import Data.Monoid
import Data.Profunctor.Unsafe
import Data.Traversable
import Data.Word
import Prelude hiding ((.),id)

------------------------------------------------------------------------------
-- Leaf
------------------------------------------------------------------------------

class Leaf i t | t -> i where
  leaf :: i -> a -> t a

------------------------------------------------------------------------------
-- Scoria
------------------------------------------------------------------------------

data Scoria i t b a where
  ScoriaAp   :: Scoria i (x -> y) b a -> Scoria i x b a -> Scoria i y b a
  ScoriaPure :: x -> Scoria i x b a
  ScoriaFmap :: (x -> y) -> Scoria i x b a -> Scoria i y b a
  ScoriaLeaf :: i -> a -> Scoria i b b a

instance Functor (Scoria i t b) where
  fmap f (ScoriaAp x y) = ScoriaAp (fmap f x) (fmap f y)
  fmap _ (ScoriaPure x) = ScoriaPure x
  fmap f (ScoriaFmap xy x) = ScoriaFmap xy (fmap f x)
  fmap f (ScoriaLeaf i a) = ScoriaLeaf i (f a)

instance Foldable (Scoria i t b) where
  foldMap f (ScoriaAp x y)   = foldMap f x `mappend` foldMap f y
  foldMap _ ScoriaPure{}     = mempty
  foldMap f (ScoriaFmap _ x) = foldMap f x
  foldMap f (ScoriaLeaf _ a) = f a

instance Traversable (Scoria i t b) where
  traverse f (ScoriaAp x y) = ScoriaAp <$> traverse f x <*> traverse f y
  traverse _ (ScoriaPure x) = pure (ScoriaPure x)
  traverse f (ScoriaFmap xy x) = ScoriaFmap xy <$> traverse f x
  traverse f (ScoriaLeaf i a) = ScoriaLeaf i <$> f a

instance b ~ t => Leaf i (Scoria i b t) where
  leaf = ScoriaLeaf

instance (Show i, Show a) => Show (Scoria i t b a) where
  showsPrec d (ScoriaAp x y) = showParen (d > 4) $
    showsPrec 4 x . showString " <*> " . showsPrec 5 y
  showsPrec d (ScoriaPure _) = showParen (d > 10) $
    showString "pure .."
  showsPrec d (ScoriaFmap _ x) = showParen (d > 4) $
    showString ".. <$> " . showsPrec 5 x
  showsPrec d (ScoriaLeaf i a) = showParen (d > 10) $
    showString "leaf " . showsPrec 11 i . showChar ' ' . showsPrec 11 a

-- | A non-reassociating initially encoded version of 'Bazaar'
newtype Molten i a b t = Molten { runMolten :: Scoria i t b a }

instance Functor (Molten i a b) where
  fmap f (Molten xs) = Molten (ScoriaFmap f xs)

instance Applicative (Molten i a b) where
  pure  = Molten #. ScoriaPure
  Molten xs <*> Molten ys = Molten (ScoriaAp xs ys)

instance (p ~ Indexed i) => Sellable p (Molten i) where
  sell = Indexed (\i -> Molten #. ScoriaLeaf i)

instance Indexable i p => Bizarre p (Molten i) where
  bazaar f (Molten (ScoriaAp x y))   = bazaar f (Molten x) <*> bazaar f (Molten y)
  bazaar f (Molten (ScoriaFmap g x)) = g <$> bazaar f (Molten x)
  bazaar _ (Molten (ScoriaPure x))   = pure x
  bazaar f (Molten (ScoriaLeaf i a)) = indexed f i a

instance IndexedFunctor (Molten i) where
  ifmap f (Molten xs) = Molten (ScoriaFmap f xs)

instance IndexedComonad (Molten i) where
  iextract (Molten (ScoriaAp x y))   = iextract (Molten x) (iextract (Molten y))
  iextract (Molten (ScoriaFmap f y)) = f (iextract (Molten y))
  iextract (Molten (ScoriaPure x))   = x
  iextract (Molten (ScoriaLeaf _ a)) = a

  iduplicate (Molten (ScoriaLeaf i a)) = Molten #. ScoriaLeaf i <$> Molten (ScoriaLeaf i a)
  iduplicate (Molten (ScoriaPure x))   = pure (pure x)
  iduplicate (Molten (ScoriaFmap f y)) = iextend (fmap f) (Molten y)
  iduplicate (Molten (ScoriaAp x y))   = iextend (<*>) (Molten x) <*> iduplicate (Molten y)

  iextend k (Molten (ScoriaLeaf i a)) = (k .# Molten) . ScoriaLeaf i <$> Molten (ScoriaLeaf i a)
  iextend k (Molten (ScoriaPure x))   = pure (k (pure x))
  iextend k (Molten (ScoriaFmap f y)) = iextend (k . fmap f) (Molten y)
  iextend k (Molten (ScoriaAp x y))   = iextend (\x' y' -> k $ x' <*> y') (Molten x) <*> iduplicate (Molten y)

instance a ~ b => Comonad (Molten i a b) where
  extract   = iextract
  extend    = iextend
  duplicate = iduplicate

------------------------------------------------------------------------------
-- Levels
------------------------------------------------------------------------------

data Level i a
  = Two {-# UNPACK #-} !Word !(Level i a) !(Level i a)
  | One i a
  | Zero
  deriving (Eq,Ord,Show,Read)

instance Leaf i (Level i) where
  leaf = One

lappend :: Level i a -> Level i a -> Level i a
lappend Zero        Zero        = Zero
lappend Zero        r@One{}     = r
lappend l@One{}     Zero        = l
lappend Zero        (Two n l r) = Two (n + 1) l r
lappend (Two n l r) Zero        = Two (n + 1) l r
lappend l           r           = Two 0 l r
{-# INLINE lappend #-}

instance Functor (Level i) where
  fmap f = go where
    go (Two n l r) = Two n (go l) (go r)
    go (One i a)   = One i (f a)
    go Zero        = Zero
  {-# INLINE fmap #-}

instance Foldable (Level i) where
  foldMap f = go where
    go (Two _ l r) = go l `mappend` go r
    go (One _ a) = f a
    go Zero = mempty
  {-# INLINE foldMap #-}

instance Traversable (Level i) where
  traverse f = go where
    go (Two n l r) = Two n <$> go l <*> go r
    go (One i a) = One i <$> f a
    go Zero = pure Zero
  {-# INLINE traverse #-}

------------------------------------------------------------------------------
-- Generating Levels
------------------------------------------------------------------------------

newtype Deepening i a = Deepening { runDeepening :: forall r. Int -> (Level i a -> Bool -> r) -> r }

-- | This is an illegal 'Monoid'.
instance Monoid (Deepening i a) where
  mempty = Deepening $ \ _ k -> k Zero False
  {-# INLINE mempty #-}
  mappend (Deepening l) (Deepening r) = Deepening $ \ n k -> case n of
    0 -> k Zero True
    _ -> let n' = n - 1 in l n' $ \x a -> r n' $ \y b -> k (lappend x y) (a || b)
  {-# INLINE mappend #-}

deepening :: i -> a -> Deepening i a
deepening i a = Deepening $ \n k -> k (if n == 0 then One i a else Zero) False
{-# INLINE deepening #-}

------------------------------------------------------------------------------
-- Reassembling Levels
------------------------------------------------------------------------------

newtype Flows i b a = Flows { runFlows :: [Level i b] -> a }

instance Functor (Flows i b) where
  fmap f (Flows g) = Flows (f . g)
  {-# INLINE fmap #-}

triml :: Level i b -> Level i b
triml (Two 0 l _) = l
triml (Two n l r) = Two (n - 1) l r
triml x           = x
{-# INLINE triml #-}

trimr :: Level i b -> Level i b
trimr (Two 0 _ r) = r
trimr (Two n l r) = Two (n - 1) l r
trimr x           = x
{-# INLINE trimr #-}

-- | This is an illegal 'Applicative'.
instance Applicative (Flows i b) where
  pure a = Flows (const a)
  {-# INLINE pure #-}
  Flows mf <*> Flows ma = Flows $ \ xss -> case xss of
    []             -> mf [] (ma [])
    (_:xs)         -> mf (triml <$> xs) $ ma (trimr <$> xs)
  {-# INLINE (<*>) #-}

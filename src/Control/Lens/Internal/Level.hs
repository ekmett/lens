{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Level
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Level
  (
  -- * Levels
    Level(..)
  , Deepening(..), deepening
  , Flows(..)
  ) where

import Control.Applicative
import Control.Category
import Control.Comonad
import Data.Foldable
import Data.Functor.Apply
import Data.Int
import Data.Semigroup
import Data.Traversable
import Data.Word
import Prelude hiding ((.),id)

------------------------------------------------------------------------------
-- Levels
------------------------------------------------------------------------------

data Level i a
  = Two {-# UNPACK #-} !Word !(Level i a) !(Level i a)
  | One i a
  | Zero
  deriving (Eq,Ord,Show,Read)

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

instance Semigroup (Deepening i a) where
  Deepening l <> Deepening r = Deepening $ \ n k -> case n of
    0 -> k Zero True
    _ -> let n' = n - 1 in l n' $ \x a -> r n' $ \y b -> k (lappend x y) (a || b)
  {-# INLINE (<>) #-}

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

instance Apply (Flows i b) where
  Flows mf <.> Flows ma = Flows $ \ xss -> case xss of
    []             -> mf [] (ma [])
    (_:xs)         -> mf (triml <$> xs) $ ma (trimr <$> xs)
  {-# INLINE (<.>) #-}

-- | This is an illegal 'Applicative'.
instance Applicative (Flows i b) where
  pure a = Flows (const a)
  {-# INLINE pure #-}
  Flows mf <*> Flows ma = Flows $ \ xss -> case xss of
    []             -> mf [] (ma [])
    (_:xs)         -> mf (triml <$> xs) $ ma (trimr <$> xs)
  {-# INLINE (<*>) #-}

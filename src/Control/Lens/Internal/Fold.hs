{-# LANGUAGE CPP #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Fold
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Fold
  (
  -- * Monoids for folding
    Folding(..)
  , Traversed(..)
  , Sequenced(..)
  , Max(..), getMax
  , Min(..), getMin
  , Leftmost(..), getLeftmost
  , Rightmost(..), getRightmost
  ) where

import Control.Applicative
import Control.Lens.Internal.Getter
import Data.Functor.Bind
import Data.Functor.Contravariant
import Data.Maybe
import Data.Semigroup hiding (Min, getMin, Max, getMax)

------------------------------------------------------------------------------
-- Folding
------------------------------------------------------------------------------

-- | A 'Monoid' for a 'Contravariant' 'Applicative'.
newtype Folding f a = Folding { getFolding :: f a }

instance (Contravariant f, Apply f) => Semigroup (Folding f a) where
  Folding fr <> Folding fs = Folding (fr .> fs)
  {-# INLINE (<>) #-}

instance (Contravariant f, Applicative f) => Monoid (Folding f a) where
  mempty = Folding noEffect
  {-# INLINE mempty #-}
  Folding fr `mappend` Folding fs = Folding (fr *> fs)
  {-# INLINE mappend #-}

------------------------------------------------------------------------------
-- Traversed
------------------------------------------------------------------------------

-- | Used internally by 'Control.Lens.Traversal.traverseOf_' and the like.
--
-- The argument 'a' of the result should not be used!
newtype Traversed a f = Traversed { getTraversed :: f a }

instance Apply f => Semigroup (Traversed a f) where
  Traversed ma <> Traversed mb = Traversed (ma .> mb)
  {-# INLINE (<>) #-}

instance Applicative f => Monoid (Traversed a f) where
  mempty = Traversed (pure (error "Traversed: value used"))
  {-# INLINE mempty #-}
  Traversed ma `mappend` Traversed mb = Traversed (ma *> mb)
  {-# INLINE mappend #-}

------------------------------------------------------------------------------
-- Sequenced
------------------------------------------------------------------------------

-- | Used internally by 'Control.Lens.Traversal.mapM_' and the like.
--
-- The argument 'a' of the result should not be used!
newtype Sequenced a m = Sequenced { getSequenced :: m a }

instance Apply m => Semigroup (Sequenced a m) where
  Sequenced ma <> Sequenced mb = Sequenced (ma .> mb)
  {-# INLINE (<>) #-}

instance Monad m => Monoid (Sequenced a m) where
  mempty = Sequenced (return (error "Sequenced: value used"))
  {-# INLINE mempty #-}
  Sequenced ma `mappend` Sequenced mb = Sequenced (ma >> mb)
  {-# INLINE mappend #-}

------------------------------------------------------------------------------
-- Min
------------------------------------------------------------------------------

-- | Used for 'Control.Lens.Fold.minimumOf'.
data Min a = NoMin | Min a

instance Ord a => Semigroup (Min a) where
  NoMin <> m     = m
  m <> NoMin     = m
  Min a <> Min b = Min (min a b)
  {-# INLINE (<>) #-}

instance Ord a => Monoid (Min a) where
  mempty = NoMin
  {-# INLINE mempty #-}
  mappend NoMin m = m
  mappend m NoMin = m
  mappend (Min a) (Min b) = Min (min a b)
  {-# INLINE mappend #-}

-- | Obtain the minimum.
getMin :: Min a -> Maybe a
getMin NoMin   = Nothing
getMin (Min a) = Just a
{-# INLINE getMin #-}

------------------------------------------------------------------------------
-- Max
------------------------------------------------------------------------------

-- | Used for 'Control.Lens.Fold.maximumOf'.
data Max a = NoMax | Max a

instance Ord a => Semigroup (Max a) where
  NoMax <> m = m
  m <> NoMax = m
  Max a <> Max b = Max (max a b)
  {-# INLINE (<>) #-}

instance Ord a => Monoid (Max a) where
  mempty = NoMax
  {-# INLINE mempty #-}
  mappend NoMax m = m
  mappend m NoMax = m
  mappend (Max a) (Max b) = Max (max a b)
  {-# INLINE mappend #-}

-- | Obtain the maximum.
getMax :: Max a -> Maybe a
getMax NoMax   = Nothing
getMax (Max a) = Just a
{-# INLINE getMax #-}

------------------------------------------------------------------------------
-- Leftmost and Rightmost
------------------------------------------------------------------------------

-- | Used for 'Control.Lens.Fold.preview'.
data Leftmost a = LPure | LLeaf a | LStep (Leftmost a)

instance Semigroup (Leftmost a) where
  (<>) = mappend
  {-# INLINE (<>) #-}

instance Monoid (Leftmost a) where
  mempty = LPure
  {-# INLINE mempty #-}
  mappend x y = LStep $ case x of
    LPure    -> y
    LLeaf _  -> x
    LStep x' -> case y of
      -- The last two cases make firstOf produce a Just as soon as any element
      -- is encountered, and possibly serve as a micro-optimisation; this
      -- behaviour can be disabled by replacing them with _ -> mappend x y'.
      -- Note that this means that firstOf (backwards folded) [1..] is Just _|_.
      LPure    -> x'
      LLeaf a  -> LLeaf $ fromMaybe a (getLeftmost x')
      LStep y' -> mappend x' y'

-- | Extract the 'Leftmost' element. This will fairly eagerly determine that it can return 'Just'
-- the moment it sees any element at all.
getLeftmost :: Leftmost a -> Maybe a
getLeftmost LPure = Nothing
getLeftmost (LLeaf a) = Just a
getLeftmost (LStep x) = getLeftmost x

-- | Used for 'Control.Lens.Fold.lastOf'.
data Rightmost a = RPure | RLeaf a | RStep (Rightmost a)

instance Semigroup (Rightmost a) where
  (<>) = mappend
  {-# INLINE (<>) #-}

instance Monoid (Rightmost a) where
  mempty = RPure
  {-# INLINE mempty #-}
  mappend x y = RStep $ case y of
    RPure    -> x
    RLeaf _  -> y
    RStep y' -> case x of
      -- The last two cases make lastOf produce a Just as soon as any element
      -- is encountered, and possibly serve as a micro-optimisation; this
      -- behaviour can be disabled by replacing them with _ -> mappend x y'.
      -- Note that this means that lastOf folded [1..] is Just _|_.
      RPure    -> y'
      RLeaf a  -> RLeaf $ fromMaybe a (getRightmost y')
      RStep x' -> mappend x' y'

-- | Extract the 'Rightmost' element. This will fairly eagerly determine that it can return 'Just'
-- the moment it sees any element at all.
getRightmost :: Rightmost a -> Maybe a
getRightmost RPure = Nothing
getRightmost (RLeaf a) = Just a
getRightmost (RStep x) = getRightmost x

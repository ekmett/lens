-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2Types
--
----------------------------------------------------------------------------
module Data.Monoid.Lens
  ( _dual, _endo, _all, _any, _sum, _product, _first, _last
  ) where

import Data.Monoid
import Control.Lens

-- | Isomorphism for 'Dual'
_dual :: Iso a b (Dual a) (Dual b)
_dual = isos Dual getDual Dual getDual
{-# INLINE _dual #-}
{-# SPECIALIZE _dual :: Functor f => Isomorphism (Dual a -> f (Dual b)) (a -> f b) #-}
{-# SPECIALIZE _dual :: Functor f => (Dual a -> f (Dual b)) -> a -> f b #-}

-- | Isomorphism for 'Endo'
_endo :: Iso (a -> a) (b -> b) (Endo a) (Endo b)
_endo = isos Endo appEndo Endo appEndo
{-# INLINE _endo #-}
{-# SPECIALIZE _endo :: Functor f => Isomorphism (Endo a -> f (Endo b)) ((a -> a) -> f (b -> b)) #-}
{-# SPECIALIZE _endo :: Functor f => (Endo a -> f (Endo b)) -> (a -> a) -> f (b -> b) #-}

-- | Isomorphism for 'All'
--
-- >>> :m + Control.Lens Data.Monoid.Lens Data.Foldable
-- >>> au _all foldMap [True,True]
-- True
--
-- >>> :m + Control.Lens Data.Monoid.Lens Data.Foldable
-- >>> au _all foldMap [True,False]
-- False
_all :: Simple Iso Bool All
_all = iso All getAll
{-# INLINE _all #-}
{-# SPECIALIZE _all :: Functor f => Isomorphism (All -> f All) (Bool -> f Bool) #-}
{-# SPECIALIZE _all :: Functor f => (All -> f All) -> Bool -> f Bool #-}

-- | Isomorphism for 'Any'
--
-- >>> :m + Control.Lens Data.Monoid.Lens Data.Foldable
-- >>> au _any foldMap [False,False]
-- False
--
-- >>> :m + Control.Lens Data.Monoid.Lens Data.Foldable
-- >>> au _any foldMap [True,False]
-- True
_any :: Simple Iso Bool Any
_any = iso Any getAny
{-# INLINE _any #-}
{-# SPECIALIZE _any :: Functor f => Isomorphism (Any -> f Any) (Bool -> f Bool) #-}
{-# SPECIALIZE _any :: Functor f => (Any -> f Any) -> Bool -> f Bool #-}

-- | Isomorphism for 'Sum'
--
-- >>> :m + Control.Lens Data.Monoid.Lens Data.Foldable
-- >>> au _sum foldMap [1,2,3,4]
-- 10
_sum :: Iso a b (Sum a) (Sum b)
_sum = isos Sum getSum Sum getSum
{-# INLINE _sum #-}
{-# SPECIALIZE _sum :: Functor f => Isomorphism (Sum a -> f (Sum b)) (a -> f b) #-}
{-# SPECIALIZE _sum :: Functor f => (Sum a -> f (Sum b)) -> a -> f b #-}

-- | Isomorphism for 'Product'
--
-- >>> :m + Control.Lens Data.Monoid.Lens Data.Foldable
-- >>> au _product foldMap [1,2,3,4]
-- 24
_product :: Iso a b (Product a) (Product b)
_product = isos Product getProduct Product getProduct
{-# INLINE _product #-}
{-# SPECIALIZE _product :: Functor f => Isomorphism (Product a -> f (Product b)) (a -> f b) #-}
{-# SPECIALIZE _product :: Functor f => (Product a -> f (Product b)) -> a -> f b #-}

-- | Isomorphism for 'First'
_first :: Iso (Maybe a) (Maybe b) (First a) (First b)
_first = isos First getFirst First getFirst
{-# INLINE _first #-}
{-# SPECIALIZE _first :: Functor f => Isomorphism (First a -> f (First b)) (Maybe a -> f (Maybe b)) #-}
{-# SPECIALIZE _first :: Functor f => (First a -> f (First b)) -> Maybe a -> f (Maybe b) #-}

-- | Isomorphism for 'Last'
_last :: Iso (Maybe a) (Maybe b) (Last a) (Last b)
_last = isos Last getLast Last getLast
{-# INLINE _last #-}
{-# SPECIALIZE _last :: Functor f => Isomorphism (Last a -> f (Last b)) (Maybe a -> f (Maybe b)) #-}
{-# SPECIALIZE _last :: Functor f => (Last a -> f (Last b)) -> Maybe a -> f (Maybe b) #-}

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
  ( (<>~), (<<>~)
  , (<>=), (<<>=)
  , _dual, _endo, _all, _any, _sum, _product, _first, _last
  ) where

import Data.Monoid
import Control.Lens
import Control.Monad.State.Class as State

-- $setup
-- >>> :m + Control.Lens Data.Monoid.Lens Data.Foldable

infixr 4 <>~, <<>~
infix 4 <>=, <<>=

-- | Modify the target of a monoidally valued by 'mappend'ing another value.
--
-- >>> both <>~ "!!!" $ ("hello","world")
-- ("hello!!!","world!!!")
--
-- @
-- ('<>~') :: 'Monoid' a => 'Setter' s t a a -> a -> s -> t
-- ('<>~') :: 'Monoid' a => 'Iso' s t a a -> a -> s -> t
-- ('<>~') :: 'Monoid' a => 'Lens' s t a a -> a -> s -> t
-- ('<>~') :: 'Monoid' a => 'Traversal' s t a a -> a -> s -> t
-- @
(<>~) :: Monoid a => Setting s t a a -> a -> s -> t
l <>~ n = over l (`mappend` n)
{-# INLINE (<>~) #-}

-- | Modify the target(s) of a 'Simple' 'Lens', 'Iso', 'Setter' or 'Traversal' by 'mappend'ing a value.
--
-- @
-- ('<>=') :: ('MonadState' s m, 'Monoid' a) => 'Simple' 'Setter' s a -> a -> m ()
-- ('<>=') :: ('MonadState' s m, 'Monoid' a) => 'Simple' 'Iso' s a -> a -> m ()
-- ('<>=') :: ('MonadState' s m, 'Monoid' a) => 'Simple' 'Lens' s a -> a -> m ()
-- ('<>=') :: ('MonadState' s m, 'Monoid' a) => 'Simple' 'Traversal' s a -> a -> m ()
-- @
(<>=) :: (MonadState s m, Monoid a) => SimpleSetting s a -> a -> m ()
l <>= a = State.modify (l <>~ a)
{-# INLINE (<>=) #-}


-- | 'mappend' a monoidal value onto the end of the target of a 'Lens' and
-- return the result
--
-- When you do not need the result of the operation, ('<>~') is more flexible.
(<<>~) :: Monoid m => LensLike ((,)m) s t m m -> m -> s -> (m, t)
l <<>~ m = l <%~ (`mappend` m)
{-# INLINE (<<>~) #-}

-- | 'mappend' a monoidal value onto the end of the target of a 'Lens' into
-- your monad's state and return the result.
--
-- When you do not need the result of the operation, ('<>=') is more flexible.
(<<>=) :: (MonadState s m, Monoid r) => SimpleLensLike ((,)r) s r -> r -> m r
l <<>= r = l <%= (`mappend` r)
{-# INLINE (<<>=) #-}

-- | Isomorphism for 'Dual'
_dual :: Iso a b (Dual a) (Dual b)
_dual = isos Dual getDual Dual getDual
{-# INLINE _dual #-}

-- | Isomorphism for 'Endo'
_endo :: Iso (a -> a) (b -> b) (Endo a) (Endo b)
_endo = isos Endo appEndo Endo appEndo
{-# INLINE _endo #-}

-- | Isomorphism for 'All'
--
-- >>> ala _all foldMap [True,True]
-- True
--
-- >>> ala _all foldMap [True,False]
-- False
_all :: Simple Iso Bool All
_all = iso All getAll
{-# INLINE _all #-}

-- | Isomorphism for 'Any'
--
-- >>> ala _any foldMap [False,False]
-- False
--
-- >>> ala _any foldMap [True,False]
-- True
_any :: Simple Iso Bool Any
_any = iso Any getAny
{-# INLINE _any #-}

-- | Isomorphism for 'Sum'
--
-- >>> ala _sum foldMap [1,2,3,4]
-- 10
_sum :: Iso a b (Sum a) (Sum b)
_sum = isos Sum getSum Sum getSum
{-# INLINE _sum #-}

-- | Isomorphism for 'Product'
--
-- >>> ala _product foldMap [1,2,3,4]
-- 24
_product :: Iso a b (Product a) (Product b)
_product = isos Product getProduct Product getProduct
{-# INLINE _product #-}

-- | Isomorphism for 'First'
_first :: Iso (Maybe a) (Maybe b) (First a) (First b)
_first = isos First getFirst First getFirst
{-# INLINE _first #-}

-- | Isomorphism for 'Last'
_last :: Iso (Maybe a) (Maybe b) (Last a) (Last b)
_last = isos Last getLast Last getLast
{-# INLINE _last #-}

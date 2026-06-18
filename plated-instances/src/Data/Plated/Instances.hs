{-# LANGUAGE FlexibleContexts #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Data.Plated.Instances
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Orphan 'Plated' instances for the @free@ and @comonad@ type families
-- (@Free@, @FreeT@, @F@, @Cofree@, @CofreeT@).
--
-- These are kept out of the @plated@ package so that depending on the 'Plated'
-- class does not drag in @free@/@comonad@. Importing this module (even as
-- @import Data.Plated.Instances ()@) brings the instances into scope.
-------------------------------------------------------------------------------
module Data.Plated.Instances () where

import Data.Plated (Plated(..))

import Control.Comonad.Cofree (Cofree(..))
import qualified Control.Comonad.Trans.Cofree as CoTrans
import Control.Monad.Free as Monad
import Control.Monad.Free.Church as Church
import Control.Monad.Trans.Free as Trans

instance Traversable f => Plated (Monad.Free f a) where
  plate f (Monad.Free as) = Monad.Free <$> traverse f as
  plate _ x         = pure x

instance (Traversable f, Traversable m) => Plated (Trans.FreeT f m a) where
  plate f (Trans.FreeT xs) = Trans.FreeT <$> traverse (traverse f) xs

instance Traversable f => Plated (Church.F f a) where
  plate f = fmap Church.toF . plate (fmap Church.fromF . f . Church.toF) . Church.fromF

instance (Traversable f, Traversable w) => Plated (CoTrans.CofreeT f w a) where
  plate f (CoTrans.CofreeT xs) = CoTrans.CofreeT <$> traverse (traverse f) xs

instance Traversable f => Plated (Cofree f a) where
  plate f (a :< as) = (:<) a <$> traverse f as

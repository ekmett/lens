{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Simple
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- Infix type operators for simple lenses and traversals.
----------------------------------------------------------------------------

module Control.Lens.Simple
  ( (:->)
  , (:=>)
  ) where

import Control.Applicative

infixr 0 :=>, :->

-- | This is a commonly used infix alias for a @'Control.Lens.Type.Simple' 'Control.Lens.Type.Lens'@.
type s :-> a = forall f. Functor f => (a -> f a) -> s -> f s

-- | This is a commonly-used infix alias for a @'Control.Lens.Type.Simple' 'Control.Lens.Traversal.Traversal'@.
type s :=> a = forall f. Applicative f => (a -> f a) -> s -> f s

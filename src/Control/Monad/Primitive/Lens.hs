{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Primitive.Lens
-- Copyright   :  (C) 2014-2026 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- 'Iso's witnessing the isomorphisms offered by "Control.Monad.Primitive".
--
-- The generic conversion between two 'PrimBase' monads that share a
-- 'PrimState' (that is, 'primToPrim' or 'liftPrim') is not given its own
-- name here: it is simply 'prim' composed with its own reverse. See 'prim'.
--
-- The @unsafe*@ conversions in "Control.Monad.Primitive" are deliberately
-- omitted: they coerce between monads with different 'PrimState' tokens,
-- and so do not form lawful 'Iso's.
----------------------------------------------------------------------------
module Control.Monad.Primitive.Lens
  ( prim
  , st
  , io
  ) where

import Control.Lens
import Control.Monad.Primitive
import Control.Monad.ST (ST)
import GHC.Exts (State#)

-- $setup
-- >>> import Control.Lens
-- >>> import Control.Monad.ST (ST, RealWorld)

-- | An 'Iso' between a 'PrimBase' monad and its underlying primitive
-- state-transformer representation.
--
-- @
-- 'view' 'prim' ≡ 'internal'
-- 'review' 'prim' ≡ 'primitive'
-- @
--
-- Composing this 'Iso' with its own reverse is the bidirectional
-- 'PrimBase'-to-'PrimBase' specialization of 'primToPrim' (or 'liftPrim'),
-- converting directly between any two 'PrimBase' monads that share a
-- 'PrimState' token. 'st' and 'io' compose the same way.
--
-- @
-- 'prim' '.' 'from' 'prim'
--   :: ('PrimBase' m, 'PrimBase' n, 'PrimState' m ~ 'PrimState' n)
--   => 'Iso' (m a) (m b) (n a) (n b)
-- @
--
-- >>> (pure 5 :: ST RealWorld Int) ^. prim . from prim :: IO Int
-- 5
prim :: PrimBase m
     => Iso (m a) (m b)
            (State# (PrimState m) -> (# State# (PrimState m), a #))
            (State# (PrimState m) -> (# State# (PrimState m), b #))
prim = iso internal primitive
{-# INLINE prim #-}

-- | An 'Iso' between a 'PrimBase' monad and the 'ST' monad with the same
-- state token.
--
-- @
-- 'view' 'st' ≡ 'primToST'
-- 'review' 'st' ≡ 'stToPrim'
-- @
--
-- >>> (pure 5 :: IO Int) ^. st . io
-- 5
st :: PrimBase m => Iso (m a) (m b) (ST (PrimState m) a) (ST (PrimState m) b)
st = iso primToST stToPrim
{-# INLINE st #-}

-- | An 'Iso' between a 'PrimBase' monad whose state token is 'RealWorld'
-- and 'IO'.
--
-- @
-- 'view' 'io' ≡ 'primToIO'
-- 'review' 'io' ≡ 'ioToPrim'
-- @
--
-- >>> (pure 5 :: ST RealWorld Int) ^. io
-- 5
io :: (PrimBase m, PrimState m ~ RealWorld) => Iso (m a) (m b) (IO a) (IO b)
io = iso primToIO ioToPrim
{-# INLINE io #-}

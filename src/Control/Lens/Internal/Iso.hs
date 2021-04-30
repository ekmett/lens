{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Iso
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Iso
  ( Exchange(..)
  , Reversing(..)
  ) where

import Data.Profunctor
import Data.Profunctor.Unsafe

import qualified Data.ByteString       as StrictB
import qualified Data.ByteString.Lazy  as LazyB
import Data.Coerce
import qualified Data.List.NonEmpty    as NonEmpty
import qualified Data.Text             as StrictT
import qualified Data.Text.Lazy        as LazyT
import qualified Data.Vector           as Vector
import qualified Data.Vector.Primitive as Prim
import Data.Vector.Primitive (Prim)
import qualified Data.Vector.Storable  as Storable
import qualified Data.Vector.Unboxed   as Unbox
import Data.Vector.Unboxed (Unbox)
import qualified Data.Sequence         as Seq
import Data.Sequence (Seq)
import Foreign.Storable (Storable)

------------------------------------------------------------------------------
-- Isomorphism: Exchange
------------------------------------------------------------------------------

-- | This is used internally by the 'Control.Lens.Iso.Iso' code to provide
-- efficient access to the two functions that make up an isomorphism.
data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Functor (Exchange a b s) where
  fmap f (Exchange sa bt) = Exchange sa (f . bt)
  {-# INLINE fmap #-}

instance Profunctor (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)
  {-# INLINE dimap #-}
  lmap f (Exchange sa bt) = Exchange (sa . f) bt
  {-# INLINE lmap #-}
  rmap f (Exchange sa bt) = Exchange sa (f . bt)
  {-# INLINE rmap #-}
  (#.) _ = coerce
  {-# INLINE (#.) #-}
  (.#) p _ = coerce p

------------------------------------------------------------------------------
-- Reversible
------------------------------------------------------------------------------

-- | This class provides a generalized notion of list reversal extended to other containers.
class Reversing t where
  reversing :: t -> t

instance Reversing [a] where
  reversing = Prelude.reverse

instance Reversing (NonEmpty.NonEmpty a) where
  reversing = NonEmpty.reverse

instance Reversing StrictB.ByteString where
  reversing = StrictB.reverse

instance Reversing LazyB.ByteString where
  reversing = LazyB.reverse

instance Reversing StrictT.Text where
  reversing = StrictT.reverse

instance Reversing LazyT.Text where
  reversing = LazyT.reverse

instance Reversing (Vector.Vector a) where
  reversing = Vector.reverse

instance Reversing (Seq a) where
  reversing = Seq.reverse

instance Prim a => Reversing (Prim.Vector a) where
  reversing = Prim.reverse

instance Unbox a => Reversing (Unbox.Vector a) where
  reversing = Unbox.reverse

instance Storable a => Reversing (Storable.Vector a) where
  reversing = Storable.reverse

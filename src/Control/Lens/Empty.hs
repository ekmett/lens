{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Empty
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-------------------------------------------------------------------------------
module Control.Lens.Empty
  (
    AsEmpty(..)
  , pattern Empty
  ) where

import Prelude ()

import Control.Lens.Iso
import Control.Lens.Fold
import Control.Lens.Prism
import Control.Lens.Internal.Prelude as Prelude
import Control.Lens.Review
import qualified Data.ByteString as StrictB
import qualified Data.ByteString.Lazy as LazyB
import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.Monoid
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as StrictT
import qualified Data.Text.Lazy as LazyT
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as Unboxed
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Storable as Storable
import Foreign.Storable (Storable)

#if !defined(mingw32_HOST_OS) && !defined(ghcjs_HOST_OS)
import GHC.Event
#endif

-- $setup
-- >>> import Control.Lens

class AsEmpty a where
  -- |
  --
  -- >>> isn't _Empty [1,2,3]
  -- True
  _Empty :: Prism' a ()
  default _Empty :: (Monoid a, Eq a) => Prism' a ()
  _Empty = only mempty
  {-# INLINE _Empty #-}

pattern Empty :: AsEmpty s => s
pattern Empty <- (has _Empty -> True) where
  Empty = review _Empty ()

{- Default Monoid instances -}
instance AsEmpty Ordering
instance AsEmpty ()
instance AsEmpty Any
instance AsEmpty All
#if !defined(mingw32_HOST_OS) && !defined(ghcjs_HOST_OS)
instance AsEmpty Event
#endif
instance (Eq a, Num a) => AsEmpty (Product a)
instance (Eq a, Num a) => AsEmpty (Sum a)

instance AsEmpty (Maybe a) where
  _Empty = _Nothing
  {-# INLINE _Empty #-}

instance AsEmpty (Last a) where
  _Empty = nearly (Last Nothing) (isNothing .# getLast)
  {-# INLINE _Empty #-}

instance AsEmpty (First a) where
  _Empty = nearly (First Nothing) (isNothing .# getFirst)
  {-# INLINE _Empty #-}

instance AsEmpty a => AsEmpty (Dual a) where
  _Empty = iso getDual Dual . _Empty
  {-# INLINE _Empty #-}

instance (AsEmpty a, AsEmpty b) => AsEmpty (a,b) where
  _Empty = prism' (\() -> (_Empty # (), _Empty # ())) $ \(s,s') -> case _Empty Left s of
    Left () -> case _Empty Left s' of
      Left () -> Just ()
      _       -> Nothing
    _         -> Nothing
  {-# INLINE _Empty #-}

instance (AsEmpty a, AsEmpty b, AsEmpty c) => AsEmpty (a,b,c) where
  _Empty = prism' (\() -> (_Empty # (), _Empty # (), _Empty # ())) $ \(s,s',s'') -> case _Empty Left s of
    Left () -> case _Empty Left s' of
      Left () -> case _Empty Left s'' of
        Left () -> Just ()
        Right _ -> Nothing
      Right _   -> Nothing
    Right _     -> Nothing
  {-# INLINE _Empty #-}

instance AsEmpty [a] where
  _Empty = nearly [] Prelude.null
  {-# INLINE _Empty #-}

instance AsEmpty (ZipList a) where
  _Empty = nearly (ZipList []) (Prelude.null . getZipList)
  {-# INLINE _Empty #-}

instance AsEmpty (Map k a) where
  _Empty = nearly Map.empty Map.null
  {-# INLINE _Empty #-}

instance AsEmpty (HashMap k a) where
  _Empty = nearly HashMap.empty HashMap.null
  {-# INLINE _Empty #-}

instance AsEmpty (IntMap a) where
  _Empty = nearly IntMap.empty IntMap.null
  {-# INLINE _Empty #-}

instance AsEmpty (Set a) where
  _Empty = nearly Set.empty Set.null
  {-# INLINE _Empty #-}

instance AsEmpty (HashSet a) where
  _Empty = nearly HashSet.empty HashSet.null
  {-# INLINE _Empty #-}

instance AsEmpty IntSet where
  _Empty = nearly IntSet.empty IntSet.null
  {-# INLINE _Empty #-}

instance AsEmpty (Vector.Vector a) where
  _Empty = nearly Vector.empty Vector.null
  {-# INLINE _Empty #-}

instance Unbox a => AsEmpty (Unboxed.Vector a) where
  _Empty = nearly Unboxed.empty Unboxed.null
  {-# INLINE _Empty #-}

instance Storable a => AsEmpty (Storable.Vector a) where
  _Empty = nearly Storable.empty Storable.null
  {-# INLINE _Empty #-}

instance AsEmpty (Seq.Seq a) where
  _Empty = nearly Seq.empty Seq.null
  {-# INLINE _Empty #-}

instance AsEmpty StrictB.ByteString where
  _Empty = nearly StrictB.empty StrictB.null
  {-# INLINE _Empty #-}

instance AsEmpty LazyB.ByteString where
  _Empty = nearly LazyB.empty LazyB.null
  {-# INLINE _Empty #-}

instance AsEmpty StrictT.Text where
  _Empty = nearly StrictT.empty StrictT.null
  {-# INLINE _Empty #-}

instance AsEmpty LazyT.Text where
  _Empty = nearly LazyT.empty LazyT.null
  {-# INLINE _Empty #-}

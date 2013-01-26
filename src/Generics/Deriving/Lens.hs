{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Deriving.Lens
-- Copyright   :  (C) 2012-13 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Note: @Generics.Deriving@ exports a number of names that collide with @Control.Lens@.
--
-- You can use hiding or imports to mitigate this to an extent, and the following imports,
-- represent a fair compromise for user code:
--
-- > import Control.Lens hiding (Rep)
-- > import Generics.Deriving hiding (from, to)
--
-- You can use 'generic' to replace 'Generics.Deriving.from' and 'Generics.Deriving.to' from @Generics.Deriving@,
-- and probably won't be explicitly referencing 'Control.Lens.Representable.Rep' from @Control.Lens@
-- in code that uses generics.
----------------------------------------------------------------------------
module Generics.Deriving.Lens
  (
  -- * Isomorphisms for @GHC.Generics@
    generic
  , generic1
  -- * Generic Traversal
  , tinplate
  , GTraversal
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Maybe (fromJust)
import           Data.Typeable
import qualified Generics.Deriving as Generic
import           Generics.Deriving hiding (from, to)

-- $setup
-- >>> :set -XNoOverloadedStrings

-- | Convert from the data type to its representation (or back)
--
-- >>> "hello"^.generic.from generic :: String
-- "hello"
generic :: Generic a => Iso' a (Generic.Rep a b)
generic = iso Generic.from Generic.to
{-# INLINE generic #-}

-- | Convert from the data type to its representation (or back)
generic1 :: Generic1 f => Iso' (f a) (Rep1 f a)
generic1 = iso from1 to1
{-# INLINE generic1 #-}

-- | A 'GHC.Generics.Generic' 'Traversal' that visits every occurrence
-- of something 'Typeable' anywhere in a container.
--
-- >>> allOf tinplate (=="Hello") (1::Int,2::Double,(),"Hello",["Hello"])
-- True
--
-- >>> mapMOf_ tinplate putStrLn ("hello",[(2 :: Int, "world!")])
-- hello
-- world!
tinplate :: (Generic a, GTraversal (Generic.Rep a), Typeable b) => Traversal' a b
tinplate = generic . tinplated True
{-# INLINE tinplate #-}

maybeArg1Of :: Maybe c -> (c -> d) -> Maybe c
maybeArg1Of = const
{-# INLINE maybeArg1Of #-}

-- | Used to traverse 'Generic' data by 'uniplate'.
class GTraversal f where
  tinplated :: Typeable b => Bool -> Traversal' (f a) b

instance (Generic a, GTraversal (Generic.Rep a), Typeable a) => GTraversal (K1 i a) where
  tinplated rec f (K1 a) = case cast a `maybeArg1Of` f of
    Just b  -> K1 . fromJust . cast <$> f b
    Nothing | rec       -> K1 <$> fmap generic (tinplated False) f a
            | otherwise -> pure $ K1 a
  {-# INLINE tinplated #-}

instance GTraversal U1 where
  tinplated _ _ U1 = pure U1
  {-# INLINE tinplated #-}

instance (GTraversal f, GTraversal g) => GTraversal (f :*: g) where
  tinplated _ f (x :*: y) = (:*:) <$> tinplated True f x <*> tinplated True f y
  {-# INLINE tinplated #-}

instance (GTraversal f, GTraversal g) => GTraversal (f :+: g) where
  tinplated _ f (L1 x) = L1 <$> tinplated True f x
  tinplated _ f (R1 x) = R1 <$> tinplated True f x
  {-# INLINE tinplated #-}

instance GTraversal a => GTraversal (M1 i c a) where
  tinplated rec f (M1 x) = M1 <$> tinplated rec f x
  {-# INLINE tinplated #-}

-- ?
instance (Traversable f, GTraversal g) => GTraversal (f :.: g) where
  tinplated _ f (Comp1 fgp) = Comp1 <$> traverse (tinplated True f) fgp
  {-# INLINE tinplated #-}

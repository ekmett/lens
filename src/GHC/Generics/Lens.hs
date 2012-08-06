{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Generics.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
----------------------------------------------------------------------------
module GHC.Generics.Lens
  (
  -- * Conversion to/from generic
    generic
  , generic1
  -- * Generic Traversal
  , every
  , GTraversal
  ) where

import Control.Applicative
import Control.Lens hiding (from, to)
import Data.Maybe (fromJust)
import Data.Typeable
import GHC.Generics

-- | Convert from the data type to its representation (or back)
generic :: (Generic a, Generic b) => Iso a b (Rep a x) (Rep b y)
generic = isos from to from to

-- | Convert from the data type to its representation (or back)
generic1 :: (Generic1 f, Generic1 g) => Iso (f a) (g b) (Rep1 f a) (Rep1 g b)
generic1 = isos from1 to1 from1 to1

-- | Traverse using GHC.Generics.
--
-- > ghci> allOf every (=="Hello") (1::Int,2::Double,(),"Hello",["Hello"])
-- True
-- > ghci> mapMOf_ every putStrLn ("hello",[(2 :: Int, "world!")])
-- > hello
-- > world!
every :: (Generic a, GTraversal (Rep a), Typeable b) => Simple Traversal a b
every = generic . everyr True

class GTraversal f where
  everyr :: Typeable b => Bool -> Simple Traversal (f a) b

instance (Generic a, GTraversal (Rep a), Typeable a) => GTraversal (K1 i a) where
  everyr rec f (K1 a) = case cast a `maybeArg1Of` f of
    Just b  -> K1 . fromJust . cast <$> f b
    Nothing | rec       -> K1 <$> fmap generic (everyr False) f a
            | otherwise -> pure $ K1 a
    where
      maybeArg1Of :: Maybe c -> (c -> d) -> Maybe c
      maybeArg1Of = const

instance GTraversal U1 where
  everyr _ _ U1 = pure U1

instance (GTraversal f, GTraversal g) => GTraversal (f :*: g) where
  everyr _ f (x :*: y) = (:*:) <$> everyr True f x <*> everyr True f y

instance (GTraversal f, GTraversal g) => GTraversal (f :+: g) where
  everyr _ f (L1 x) = L1 <$> everyr True f x
  everyr _ f (R1 x) = R1 <$> everyr True f x

instance GTraversal a => GTraversal (M1 i c a) where
  everyr rec f (M1 x) = M1 <$> everyr rec f x

-- ?
instance (Traversable f, GTraversal g) => GTraversal (f :.: g) where
  everyr _ f (Comp1 fgp) = Comp1 <$> traverse (everyr True f) fgp


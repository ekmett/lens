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
-- Note: @GHC.Generics@ exports a number of names that collide with @Control.Lens@.
--
-- You can use hiding or imports to mitigate this to an extent, and the following imports,
-- represent a fair compromise for user code:
--
-- > import Control.Lens hiding (Rep)
-- > import GHC.Generics hiding (from, to)
--
-- You can use 'generic' to replace 'GHC.Generics.from' and 'GHC.Generics.to' from @GHC.Generics@,
-- and probably won't be explicitly referencing 'Control.Lens.Representable.Rep' from @Control.Lens@
-- in code that uses generics.
----------------------------------------------------------------------------
module GHC.Generics.Lens
  (
  -- * Isomorphisms for @GHC.Generics@
    generic
  , generic1
  -- * 'Generic' 'Traversal'
  , every
  , GTraversal
  ) where

import           Control.Applicative
import           Control.Lens hiding (Rep)
import           Data.Maybe (fromJust)
import           Data.Typeable
import qualified GHC.Generics as Generic
import           GHC.Generics                     hiding (from, to)

-- | Convert from the data type to its representation (or back)
--
-- >>> "hello"^.generic.from generic :: String
-- "hello"
--
generic :: Generic a => Simple Iso a (Rep a b)
generic = iso Generic.from Generic.to

-- | Convert from the data type to its representation (or back)
generic1 :: Generic1 f => Simple Iso (f a) (Rep1 f a)
generic1 = iso from1 to1

-- | A 'GHC.Generics.Generic' 'Traversal' that visits every occurence
-- of something 'Typeable' anywhere in a container.
--
-- >>> allOf every (=="Hello") (1::Int,2::Double,(),"Hello",["Hello"])
-- True
--
-- >>> mapMOf_ every putStrLn ("hello",[(2 :: Int, "world!")])
-- hello
-- world!
every :: (Generic a, GTraversal (Rep a), Typeable b) => Simple Traversal a b
every = generic . everyr True

-- | Used to traverse 'Generic' data by 'every'.
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


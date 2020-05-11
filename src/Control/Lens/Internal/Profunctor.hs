module Control.Lens.Internal.Profunctor 
  ( WrappedPafb (..)
  ) where

import Prelude ()
import Control.Lens.Internal.Prelude

newtype WrappedPafb f p a b = WrapPafb { unwrapPafb :: p a (f b) }

instance (Functor f, Profunctor p) => Profunctor (WrappedPafb f p) where
  dimap f g (WrapPafb p) = WrapPafb $ dimap f (fmap g) p

instance (Applicative f, Choice p) => Choice (WrappedPafb f p) where
  left' (WrapPafb p) = WrapPafb $ rmap sequenceL $ left' p
    where
      sequenceL (Left a) = fmap Left a
      sequenceL (Right a) = pure $ Right a

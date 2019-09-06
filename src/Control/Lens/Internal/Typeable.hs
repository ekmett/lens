{-# language CPP #-}
{-# language TypeOperators #-}
{-# language ScopedTypeVariables #-}
module Control.Lens.Internal.Typeable (
    eqT
  , typeRep
  ) where
#if MIN_VERSION_base (4,7,0)
import Data.Typeable (eqT, typeRep)

#else

import Data.Type.Equality
import Data.Typeable (Typeable, TypeRep, gcast, typeOf)

-- | Extract a witness of equality of two types
eqT :: (Typeable a, Typeable b) => Maybe (a :~: b)
eqT = gcast Refl

-- | Takes a value of type @a@ and returns a concrete representation of that type.
typeRep :: forall proxy a . Typeable a => proxy a -> TypeRep
typeRep _ = typeOf (undefined :: a)

#endif

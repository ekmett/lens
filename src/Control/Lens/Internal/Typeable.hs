{-# language CPP #-}
{-# language TypeOperators #-}
module Control.Lens.Internal.Typeable (
    eqT
  ) where
#if MIN_VERSION_base (4,7,0)
import Data.Typeable (eqT)

#else

import Data.Type.Equality
import Data.Typeable (Typeable, gcast)

-- | Extract a witness of equality of two types
eqT :: (Typeable a, Typeable b) => Maybe (a :~: b)
eqT = gcast Refl

#endif

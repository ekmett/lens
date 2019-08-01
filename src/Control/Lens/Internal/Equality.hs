{-# language CPP #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
{-# language TypeOperators #-}
#if __GLASGOW_HASKELL__ >= 706
-- PolyKinds is known to be especially flaky in 7.4. It didn't get *really*
-- solid until 8.0 or, but it's somewhat usable from 7.6 to 7.10.
{-# language PolyKinds #-}
#endif
module Control.Lens.Internal.Equality (
    (:~:)(..)
  , sym
  , trans
  , castWith
  , gcastWith
  , eqT

{-
We can't define 'apply' or 'outer' for 7.4, since they won't kind-check without
PolyKinds. We're not currently using them anyway.

'inner' won't typecheck with 7.4 or 7.6, apparently due to GHC bugs.  There
may be a workaround available if we need it:

  https://github.com/ekmett/lens/pull/862#discussion_r307945760
-}
  ) where
#if MIN_VERSION_base (4,7,0)
import Data.Type.Equality
import Data.Typeable (eqT)

#else

import Control.Category
import Data.Typeable (Typeable, gcast)
import Prelude hiding (id, (.))

infix 4 :~:

-- | Propositional equality. If @a :~: b@ is inhabited by some terminating
-- value, then the type @a@ is the same as the type @b@. To use this equality
-- in practice, pattern-match on the @a :~: b@ to get out the @Refl@ constructor;
-- in the body of the pattern-match, the compiler knows that @a ~ b@.
data a :~: b where
  Refl :: a :~: a

instance Category (:~:) where
  id = Refl
  Refl . Refl = Refl

-- | Symmetry of equality
sym :: (a :~: b) -> (b :~: a)
sym Refl = Refl

-- | Transitivity of equality
trans :: (a :~: b) -> (b :~: c) -> (a :~: c)
trans Refl Refl = Refl

-- | Type-safe cast, using propositional equality
castWith :: (a :~: b) -> a -> b
castWith Refl x = x

-- | Generalized form of type-safe cast using propositional equality
gcastWith :: (a :~: b) -> ((a ~ b) => r) -> r
gcastWith Refl x = x

deriving instance Eq   (a :~: b)
deriving instance Ord  (a :~: b)
deriving instance Show (a :~: b)
deriving instance a ~ b => Read (a :~: b)

-- This can't be derived until 7.8, when we don't need it anymore.
instance a ~ b => Bounded (a :~: b) where
  minBound = Refl
  maxBound = Refl

instance a ~ b => Enum (a :~: b) where
  toEnum 0 = Refl
  toEnum _ = error "Control.Lens.Internal.Equality toEnum: bad argument"

  fromEnum Refl = 0

-- | Extract a witness of equality of two types
eqT :: (Typeable a, Typeable b) => Maybe (a :~: b)
eqT = gcast Refl
#endif

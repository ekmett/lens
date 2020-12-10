{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 800 && __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif
module T917 where

import Control.Lens
import Data.Proxy

#if __GLASGOW_HASKELL__ >= 800 && __GLASGOW_HASKELL__ < 806
import Data.Kind
#endif

-- Like Data.Functor.Const, but redfined to ensure that it is poly-kinded
-- across all versions of GHC, not just 8.0+
newtype Constant a (b :: k) = Constant a

data T917OneA (a :: k -> *) (b :: k -> *) = MkT917OneA
data T917OneB a b = MkT917OneB (T917OneA a (Const b))
$(makePrisms ''T917OneB)

data T917TwoA (a :: k -> *) (b :: k -> *) = MkT917TwoA
data T917TwoB a b = MkT917TwoB (T917TwoA a (Const b))
$(makeClassyPrisms ''T917TwoB)

data family   T917DataFam (a :: k)
data instance T917DataFam (a :: *) = MkT917DataFam { _unT917DataFam :: Proxy a }
$(makeLenses 'MkT917DataFam)

#if __GLASGOW_HASKELL__ >= 800
data T917GadtOne (a :: k) where
  MkT917GadtOne :: T917GadtOne (a :: *)
$(makePrisms ''T917GadtOne)

data T917GadtTwo (a :: k) where
  MkT917GadtTwo :: T917GadtTwo (a :: *)
$(makePrisms ''T917GadtTwo)
#endif

{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Unsafe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Exception
-- Copyright   :  (C) 2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Exception
  ( Handling(..)
  ) where

import Control.Exception
import Data.Reflection
import Data.Typeable
import Data.Proxy
import System.IO.Unsafe
import Data.IORef

supply :: IORef Int
supply = unsafePerformIO $ newIORef 0
{-# NOINLINE supply #-}

-- | This permits the construction of a 'Control.Exception.Handler' that matches only if some function does.
newtype Handling a s (m :: * -> *) = Handling a

-- the m parameter exists simply to break the Typeable1 pattern, so we can provide this without overlap.
-- here we simply generate a fresh TypeRep so we'll fail to compare as equal to any other TypeRep.
instance Typeable (Handling a s m) where
  typeOf _ = unsafePerformIO $ do
    i <- atomicModifyIORef supply $ \a -> let a' = a + 1 in a' `seq` (a', a)
    return $ mkTyConApp (mkTyCon3 "lens" "Control.Lens.Internal.Exception" ("Handling" ++ show i)) []
  {-# INLINE typeOf #-}

-- The 'Handling' wrapper is uninteresting, and is never thrown, so you won't get much benefit here.
instance Show (Handling a s m) where
  showsPrec d _ = showParen (d > 10) $ showString "Handling ..."
  {-# INLINE showsPrec #-}

instance Reifies s (SomeException -> Maybe a) => Exception (Handling a s m) where
  toException = error "Handling: toException"
  {-# INLINE toException #-}
  fromException = fmap Handling . reflect (Proxy :: Proxy s)
  {-# INLINE fromException #-}

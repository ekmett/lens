{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dynamic.Lens
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.Dynamic.Lens
  ( AsDynamic(..)
  , pattern Data.Dynamic.Lens.Dynamic
  ) where

import Control.Exception
import Control.Exception.Lens
import Control.Lens
import Data.Dynamic

-- | Any t'Dynamic' can be thrown as an t'Exception'
class AsDynamic t where
  -- | This t'Prism' allows you to traverse the typed value contained in a
  -- t'Dynamic' where the type required by your function matches that
  -- of the contents of the t'Dynamic', or construct a t'Dynamic' value
  -- out of whole cloth. It can also be used to catch or throw a t'Dynamic'
  -- value as 'SomeException'.
  --
  -- @
  -- '_Dynamic' :: 'Typeable' a => 'Prism'' t'Dynamic'      a
  -- '_Dynamic' :: 'Typeable' a => 'Prism'' 'SomeException' a
  -- @
  _Dynamic :: Typeable a => Prism' t a

instance AsDynamic Dynamic where
  _Dynamic = prism' toDyn fromDynamic
  {-# INLINE _Dynamic #-}

instance AsDynamic SomeException where
  _Dynamic = exception.prism' toDyn fromDynamic
  {-# INLINE _Dynamic #-}

pattern Dynamic :: (AsDynamic s, Typeable a) => a -> s
pattern Dynamic a <- (preview _Dynamic -> Just a) where
  Dynamic a = review _Dynamic a

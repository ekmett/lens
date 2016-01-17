{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Review
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Review
  (
  -- * Internal Classes
    Reviewable
  -- * Reviews
  , retagged
  ) where

import Data.Bifunctor
import Data.Profunctor
import Data.Void

-- | This class is provided mostly for backwards compatibility with lens 3.8,
-- but it can also shorten type signatures.
class (Profunctor p, Bifunctor p) => Reviewable p
instance (Profunctor p, Bifunctor p) => Reviewable p

------------------------------------------------------------------------------
-- Review: Reviewed
------------------------------------------------------------------------------

-- | This is a profunctor used internally to implement "Review"
--
-- It plays a role similar to that of 'Control.Lens.Internal.Getter.Accessor'
-- or 'Const' do for "Control.Lens.Getter"
retagged :: (Profunctor p, Bifunctor p) => p a b -> p s b
retagged = first absurd . lmap absurd

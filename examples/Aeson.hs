{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- This is a small example of how to construct a projection for a third-party library like
-- @aeson@.
--
-- To test this:
--
-- > doctest Aeson.hs
module Aeson where

import Control.Lens
import Data.Aeson
import Data.ByteString.Lazy

-- |
-- >>> review aeson 5
-- "5"
-- >>> [1,2,3]^.re aeson
-- "[1,2,3]"
-- >>> aeson.both +~ 2 $ (2,3)^.re aeson
-- "[4,5]"
aeson, aeson' :: (FromJSON a, ToJSON a) => Prism' ByteString a
aeson  = prism' encode decode
aeson' = prism' encode decode'

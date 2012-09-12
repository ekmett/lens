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
-- >>> 5^.by aeson
-- "5"
-- >>> [1,2,3]^.by aeson
-- "[1,2,3]"
-- >>> aeson.both +~ 2 $ (2,3)^.by aeson
-- "[4,5]"
aeson, aeson' :: (FromJSON c, ToJSON d) => Projection ByteString ByteString c d
aeson  = projection encode decode
aeson' = projection encode decode'

{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

#if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif
module T972 where

import Control.Lens
import Data.Proxy

newtype Arc s = Arc { _unArc :: Int }

data Direction = Negative | Positive
data Dart s = Dart { _arc :: Arc s, _direction :: Direction }
$(makeLenses ''Dart)

data Fancy k (a :: k) = MkFancy { _unFancy1 :: k, _unFancy2 :: Proxy a }
$(makeLenses ''Fancy)

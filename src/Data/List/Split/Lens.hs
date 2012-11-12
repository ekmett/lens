{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
module Data.List.Split.Lens
  ( Splitter
  , keepsDelims
  , keepsBlanks
  ) where


import Control.Applicative
import Control.Lens
import Control.Monad.State as State (MonadState, modify)
import Data.List.Split.Internal (Splitter(..), DelimPolicy(..), CondensePolicy(..), EndPolicy(..))

keepsDelims :: SimpleLens (Splitter a) (Bool, Bool)
keepsDelims = lens
              (\ Splitter{delimPolicy=dp} -> case dp of
                  Drop      -> (False, False)
                  Keep      -> (True , True )
                  KeepLeft  -> (True , False)
                  KeepRight -> (False, True ))
              (\s bb -> s{delimPolicy=(case bb of
                                          (False, False) -> Drop
                                          (True , True ) -> Keep
                                          (True , False) -> KeepLeft
                                          (False, True ) -> KeepRight)})

keepsInternalBlanks

{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
module Data.List.Split.Lens
  ( Splitter
  , delim
  , keepsDelims
  , keepsBlanks
  ) where


import Control.Applicative
import Control.Lens
import Data.List.Split.Internals

delim :: Lens (Splitter a) (Splitter b) [a -> Bool] [b -> Bool]
delim f s@(Splitter{delimiter=Delimiter ds}) = (\ds' -> s{delimiter=Delimiter ds'}) <$> f ds

keepsDelims :: Simple Lens (Splitter a) (Bool, Bool)
keepsDelims = lens
              (\ Splitter{delimPolicy=dp} -> case dp of
                  Drop      -> (False, False)
                  Keep      -> (True , True )
                  KeepRight -> (True , False) -- see the docs for the @split@ package as to why this looks backwards
                  KeepLeft  -> (False, True ))
              (\ s bb -> s{delimPolicy=(case bb of
                                           (False, False) -> Drop
                                           (True , True ) -> Keep
                                           (True , False) -> KeepRight
                                           (False, True ) -> KeepLeft)})

keepsBlanks :: Simple Lens (Splitter a) (Bool, Bool, Bool)
keepsBlanks = lens
              (\ Splitter{condensePolicy=cp,initBlankPolicy=ibp,finalBlankPolicy=fbp} -> (ibp==KeepBlank      ,
                                                                                          cp ==KeepBlankFields,
                                                                                          fbp==KeepBlank      ))
              (\ s (bI, bC, bF) -> s{condensePolicy  =if bC then KeepBlankFields else Condense ,
                                     initBlankPolicy =if bI then KeepBlank       else DropBlank,
                                     finalBlankPolicy=if bF then KeepBlank       else DropBlank})

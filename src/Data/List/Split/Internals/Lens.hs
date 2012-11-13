{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
module Data.List.Split.Internals.Lens
  ( delimiters
  , delimiting
  , condensing
  , initialBlanks
  , finalBlanks
  ) where

import Control.Applicative
import Control.Lens
import Data.List.Split.Internals

-- | Modify or retrieve the list of delimiters for a 'Splitter'.
delimiters :: Lens (Splitter a) (Splitter b) [a -> Bool] [b -> Bool]
delimiters f s@Splitter { delimiter = Delimiter ds } = (\ds' -> s { delimiter = Delimiter ds' }) <$> f ds

-- | Modify or retrieve the policy for what a 'Splitter' to do with delimiters.
delimiting :: Simple Lens (Splitter a) DelimPolicy
delimiting f s@Splitter { delimPolicy = p } = (\p' -> s { delimPolicy = p' }) <$> f p

-- | Modify or retrieve the policy for what a 'Splitter' should about consecutive delimiters.
condensing :: Simple Lens (Splitter a) CondensePolicy
condensing f s@Splitter { condensePolicy = p } = (\p' -> s { condensePolicy = p' }) <$> f p

-- | Modify or retrieve the policy for whether a 'Splitter' should drop an initial blank.
initialBlanks :: Simple Lens (Splitter a) EndPolicy
initialBlanks f s@Splitter { initBlankPolicy = p } = (\p' -> s { initBlankPolicy = p' }) <$> f p

-- | Modify or retrieve the policy for whether a 'Splitter' should drop a final blank.
finalBlanks :: Simple Lens (Splitter a) EndPolicy
finalBlanks f s@Splitter { finalBlankPolicy = p } = (\p' -> s { finalBlankPolicy = p' }) <$> f p

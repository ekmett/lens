{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
module Data.List.Split.Internals.Lens
  ( delimiters
  , delimiting
  , condensing
  , keepInitialBlanks
  , keepFinalBlanks
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
condensing :: Simple Lens (Splitter a) Bool
condensing f s@Splitter { condensePolicy = p } = (\p' -> s { condensePolicy = i p' }) <$> f (o p) where
  i True = Condense
  i False = KeepBlankFields
  o Condense = True
  o KeepBlankFields = False

-- | Modify or retrieve the policy for whether a 'Splitter' should drop an initial blank.
keepInitialBlanks :: Simple Lens (Splitter a) Bool
keepInitialBlanks f s@Splitter { initBlankPolicy = p } = (\p' -> s { initBlankPolicy = end p' }) <$> f (keeps p)

-- | Modify or retrieve the policy for whether a 'Splitter' should drop a final blank.
keepFinalBlanks :: Simple Lens (Splitter a) Bool
keepFinalBlanks f s@Splitter { finalBlankPolicy = p } = (\p' -> s { finalBlankPolicy = end p' }) <$> f (keeps p)

-- utilities

end :: Bool -> EndPolicy
end True  = KeepBlank
end False = DropBlank

keeps :: EndPolicy -> Bool
keeps KeepBlank = True
keeps DropBlank = False

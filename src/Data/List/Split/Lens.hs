{-# LANGUAGE Rank2Types #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Split.Lens
-- Copyright   :  (C) 2012-2013 Edward Kmett, Alexander Altman
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- Lenses for working with Data.List.Split
--
----------------------------------------------------------------------------
module Data.List.Split.Lens
  (
  -- * Splitting Folds
    splitting
  , splittingOn
  , splittingOneOf
  , splittingWhen
  , endingBy
  , endingByOneOf
  , wordingBy
  , liningBy
  , chunking
  , splittingPlaces
  , splittingPlacesBlanks
  -- * Lenses for 'Splitter' Internals
  , delimiters
  , delimiting
  , condensing
  , keepInitialBlanks
  , keepFinalBlanks
  ) where

import Control.Lens
import Data.Monoid
import Data.List.Split
import Data.List.Split.Internals

-- $setup
-- >>> import Control.Lens

-- | Obtain a 'Fold' by splitting another 'Fold', 'Lens', 'Getter' or 'Traversal' according to the given splitting strategy.
--
-- @
-- 'splitting' :: 'Splitter' a -> 'Fold' s a -> 'Fold' s [a]
-- @
splitting :: Splitter a -> Getting (Endo [a]) s a -> Fold s [a]
splitting s l f = coerce . traverse f . split s . toListOf l
{-# INLINE splitting #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Lens', 'Getter' or 'Traversal' on the given delimiter.
--
-- Equivalent to @'splitting' '.' 'dropDelims' '.' 'onSublist'@.
--
-- @
-- 'splittingOn' :: 'Eq' a => [a] -> 'Fold' s a -> 'Fold' s [a]
-- @
splittingOn :: Eq a => [a] -> Getting (Endo [a]) s a -> Fold s [a]
splittingOn s l f = coerce . traverse f . splitOn s . toListOf l
{-# INLINE splittingOn #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Lens', 'Getter' or 'Traversal' on any of the given elements.
--
-- Equivalent to @'splitting' '.' 'dropDelims' '.' 'oneOf'@.
--
-- @
-- 'splittingOn' :: 'Eq' a => [a] -> 'Fold' s a -> 'Fold' s [a]
-- @
splittingOneOf :: Eq a => [a] -> Getting (Endo [a]) s a -> Fold s [a]
splittingOneOf s l f = coerce . traverse f . splitOneOf s . toListOf l
{-# INLINE splittingOneOf #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Lens', 'Getter' or 'Traversal' on elements satisfying the given predicate.
--
-- Equivalent to @'splitting' '.' 'dropDelims' '.' 'whenElt'@.
--
-- @
-- 'splittingWhen' :: (a -> 'Bool') -> 'Fold' s a -> 'Fold' s [a]
-- @
splittingWhen :: (a -> Bool) -> Getting (Endo [a]) s a -> Fold s [a]
splittingWhen s l f = coerce . traverse f . splitWhen s . toListOf l
{-# INLINE splittingWhen #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Lens', 'Getter' or 'Traversal' into chunks terminated by the given delimiter.
--
-- Equivalent to @'splitting' '.' 'dropDelims' '.' 'onSublist'@.
--
-- @
-- 'endingBy' :: 'Eq' a => [a] -> 'Fold' s a -> 'Fold' s [a]
-- @
endingBy :: Eq a => [a] -> Getting (Endo [a]) s a -> Fold s [a]
endingBy s l f = coerce . traverse f . endBy s . toListOf l
{-# INLINE endingBy #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Lens', 'Getter' or 'Traversal' into chunks terminated by any of the given elements.
--
-- Equivalent to @'splitting' '.' 'dropFinalBlank' '.' 'dropDelims' '.' 'oneOf'@.
--
-- @
-- 'endingByOneOf' :: 'Eq' a => [a] -> 'Fold' s a -> 'Fold' s [a]
-- @
endingByOneOf :: Eq a => [a] -> Getting (Endo [a]) s a -> Fold s [a]
endingByOneOf s l f = coerce . traverse f . endByOneOf s . toListOf l
{-# INLINE endingByOneOf #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Lens', 'Getter' or 'Traversal' into "words", with word boundaries indicated by the given predicate.
--
-- Equivalent to @'splitting' '.' 'dropBlanks' '.' 'dropDelims' '.' 'whenElt'@.
--
-- @
-- 'wordingBy' :: (a -> 'Bool') -> 'Fold' s a -> 'Fold' s [a]
-- @
wordingBy :: (a -> Bool) -> Getting (Endo [a]) s a -> Fold s [a]
wordingBy s l f = coerce . traverse f . wordsBy s . toListOf l
{-# INLINE wordingBy #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Lens', 'Getter' or 'Traversal' into "lines", with line boundaries indicated by the given predicate.
--
-- Equivalent to @'splitting' '.' 'dropFinalBlank' '.' 'dropDelims' '.' 'whenElt'@.
--
-- @
-- 'liningBy' :: (a -> 'Bool') -> 'Fold' s a -> 'Fold' s [a]
-- @
liningBy :: (a -> Bool) -> Getting (Endo [a]) s a -> Fold s [a]
liningBy s l f = coerce . traverse f . linesBy s . toListOf l
{-# INLINE liningBy #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Lens', 'Getter' or 'Traversal' into length-@n@ pieces.
--
-- @
-- 'chunking' :: 'Int' -> 'Fold' s a -> 'Fold' s [a]
-- @
chunking :: Int -- ^ @n@
            -> Getting (Endo [a]) s a -> Fold s [a]
chunking s l f = coerce . traverse f . chunksOf s . toListOf l
{-# INLINE chunking #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Lens', 'Getter' or 'Traversal' into chunks of the given lengths, .
--
-- @
-- 'splittingPlaces' :: 'Integral' n => [n] -> 'Fold' s a -> 'Fold' s [a]
-- @
splittingPlaces :: Integral n => [n] -> Getting (Endo [a]) s a -> Fold s [a]
splittingPlaces s l f = coerce . traverse f . splitPlaces s . toListOf l
{-# INLINE splittingPlaces #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Lens', 'Getter' or 'Traversal' into chunks of the given lengths.  Unlike 'splittingPlaces', the output 'Fold' will always be the same length as the first input argument.
--
-- @
-- 'splittingPlacesBlanks' :: 'Integral' n => [n] -> 'Fold' s a -> 'Fold' s [a]
-- @
splittingPlacesBlanks :: Integral n => [n] -> Getting (Endo [a]) s a -> Fold s [a]
splittingPlacesBlanks s l f = coerce . traverse f . splitPlacesBlanks s . toListOf l
{-# INLINE splittingPlacesBlanks #-}

-- | Modify or retrieve the list of delimiters for a 'Splitter'.
delimiters :: Lens (Splitter a) (Splitter b) [a -> Bool] [b -> Bool]
delimiters f s@Splitter { delimiter = Delimiter ds } = f ds <&> \ds' -> s { delimiter = Delimiter ds' }
{-# INLINE delimiters #-}

-- | Modify or retrieve the policy for what a 'Splitter' to do with delimiters.
delimiting :: Lens' (Splitter a) DelimPolicy
delimiting f s@Splitter { delimPolicy = p } = f p <&> \p' -> s { delimPolicy = p' }
{-# INLINE delimiting #-}

-- | Modify or retrieve the policy for what a 'Splitter' should about consecutive delimiters.
condensing :: Lens' (Splitter a) Bool
condensing f s@Splitter { condensePolicy = p } = f (o p) <&> \p' -> s { condensePolicy = i p' } where
  i True = Condense
  i False = KeepBlankFields
  o Condense = True
  o KeepBlankFields = False
{-# INLINE condensing #-}

-- | Modify or retrieve the policy for whether a 'Splitter' should drop an initial blank.
keepInitialBlanks :: Lens' (Splitter a) Bool
keepInitialBlanks f s@Splitter { initBlankPolicy = p } = f (keeps p) <&> \p' -> s { initBlankPolicy = end p' }
{-# INLINE keepInitialBlanks #-}

-- | Modify or retrieve the policy for whether a 'Splitter' should drop a final blank.
keepFinalBlanks :: Lens' (Splitter a) Bool
keepFinalBlanks f s@Splitter { finalBlankPolicy = p } = f (keeps p) <&> \p' -> s { finalBlankPolicy = end p' }
{-# INLINE keepFinalBlanks #-}

-- utilities

end :: Bool -> EndPolicy
end True  = KeepBlank
end False = DropBlank
{-# INLINE end #-}

keeps :: EndPolicy -> Bool
keeps KeepBlank = True
keeps DropBlank = False
{-# INLINE keeps #-}

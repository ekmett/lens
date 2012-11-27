{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Split.Lens
-- Copyright   :  (C) 2012 Edward Kmett, Alexander Altman
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

import Control.Applicative
import Control.Lens
import Control.Lens.Classes
import Data.Monoid
import Data.List.Split
import Data.List.Split.Internals

-- $setup
-- >>> import Control.Lens

-- | Obtain a 'Fold' by splitting another 'Fold', 'Control.Lens.Type.Lens', 'Getter' or 'Control.Lens.Traversal.Traversal' according to the given splitting strategy.
--
-- @
-- 'splitting' :: 'Splitter' a -> 'Fold' i s a -> 'Fold' [i] s [a]
-- @
splitting :: (Applicative f, Gettable f) => Splitter a -> Getting (Endo [a]) s s a a -> LensLike f s s [a] [a]
splitting s l f = coerce . traverse f . split s . toListOf l
{-# INLINE splitting #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Control.Lens.Type.Lens', 'Getter' or 'Control.Lens.Traversal.Traversal' on the given delimiter.
--
-- Equivalent to @'splitting' '.' 'dropDelims' '.' 'onSublist'@.
--
-- @
-- 'splittingOn' :: 'Eq' a => [a] -> 'Fold' s a -> 'Fold' s [a]
-- @
splittingOn :: (Applicative f, Gettable f, Eq a) => [a] -> Getting (Endo [a]) s s a a -> LensLike f s s [a] [a]
splittingOn s l f = coerce . traverse f . splitOn s . toListOf l
{-# INLINE splittingOn #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Control.Lens.Type.Lens', 'Getter' or 'Control.Lens.Traversal.Traversal' on any of the given elements.
--
-- Equivalent to @'splitting' '.' 'dropDelims' '.' 'oneOf'@.
--
-- @
-- 'splittingOn' :: 'Eq' a => [a] -> 'Fold' s a -> 'Fold' s [a]
-- @
splittingOneOf :: (Applicative f, Gettable f, Eq a) => [a] -> Getting (Endo [a]) s s a a -> LensLike f s s [a] [a]
splittingOneOf s l f = coerce . traverse f . splitOneOf s . toListOf l
{-# INLINE splittingOneOf #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Control.Lens.Type.Lens', 'Getter' or 'Control.Lens.Traversal.Traversal' on elements satisfying the given predicate.
--
-- Equivalent to @'splitting' '.' 'dropDelims' '.' 'whenElt'@.
--
-- @
-- 'splittingOn' :: (a -> 'Bool') -> 'Fold' s a -> 'Fold' s [a]
-- @
splittingWhen :: (Applicative f, Gettable f, Eq a) => (a -> Bool) -> Getting (Endo [a]) s s a a -> LensLike f s s [a] [a]
splittingWhen s l f = coerce . traverse f . splitWhen s . toListOf l
{-# INLINE splittingWhen #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Control.Lens.Type.Lens', 'Getter' or 'Control.Lens.Traversal.Traversal' into chunks terminated by the given delimiter.
--
-- Equivalent to @'splitting' '.' 'dropDelims' '.' 'onSublist'@.
--
-- @
-- 'endingBy' :: 'Eq' a => [a] -> 'Fold' s a -> 'Fold' s [a]
-- @
endingBy :: (Applicative f, Gettable f, Eq a) => [a] -> Getting (Endo [a]) s s a a -> LensLike f s s [a] [a]
endingBy s l f = coerce . traverse f . endBy s . toListOf l
{-# INLINE endingBy #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Control.Lens.Type.Lens', 'Getter' or 'Control.Lens.Traversal.Traversal' into chunks terminated by any of the given elements.
--
-- Equivalent to @'splitting' '.' 'dropFinalBlank' '.' 'dropDelims' '.' 'oneOf'@.
--
-- @
-- 'endingByOneOf' :: 'Eq' a => [a] -> 'Fold' s a -> 'Fold' s [a]
-- @
endingByOneOf :: (Applicative f, Gettable f, Eq a) => [a] -> Getting (Endo [a]) s s a a -> LensLike f s s [a] [a]
endingByOneOf s l f = coerce . traverse f . endByOneOf s . toListOf l
{-# INLINE endingByOneOf #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Control.Lens.Type.Lens', 'Getter' or 'Control.Lens.Traversal.Traversal' into "words", with word boundaries indicated by the given predicate.
--
-- Equivalent to @'splitting' '.' 'dropBlanks' '.' 'dropDelims' '.' 'whenElt'@.
--
-- @
-- 'wordingBy' :: (a -> 'Bool') -> 'Fold' a -> 'Fold' s [a]
-- @
wordingBy :: (Applicative f, Gettable f, Eq a) => (a -> Bool) -> Getting (Endo [a]) s s a a -> LensLike f s s [a] [a]
wordingBy s l f = coerce . traverse f . wordsBy s . toListOf l
{-# INLINE wordingBy #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Control.Lens.Type.Lens', 'Getter' or 'Control.Lens.Traversal.Traversal' into "lines", with line boundaries indicated by the given predicate.
--
-- Equivalent to @'splitting' '.' 'dropFinalBlank' '.' 'dropDelims' '.' 'whenElt'@.
--
-- @
-- 'liningBy' :: (a -> 'Bool') -> 'Fold' s a -> 'Fold' s [a]
-- @
liningBy :: (Applicative f, Gettable f, Eq a) => (a -> Bool) -> Getting (Endo [a]) s s a a -> LensLike f s s [a] [a]
liningBy s l f = coerce . traverse f . linesBy s . toListOf l
{-# INLINE liningBy #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Control.Lens.Type.Lens', 'Getter' or 'Control.Lens.Traversal.Traversal' into length-@n@ pieces.
--
-- @
-- 'chunkingOf' :: 'Int' -> 'Fold' s a -> 'Fold' s [a]
-- @
chunking :: (Applicative f, Gettable f) => Int -- ^ @n@
            -> Getting (Endo [a]) s s a a -> LensLike f s s [a] [a]
chunking s l f = coerce . traverse f . chunksOf s . toListOf l
{-# INLINE chunking #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Control.Lens.Type.Lens', 'Getter' or 'Control.Lens.Traversal.Traversal' into chunks of the given lengths, .
--
-- @
-- 'splittingPlaces' :: 'Integral' n => [n] -> 'Fold' s a -> 'Fold' s [a]
-- @
splittingPlaces :: (Applicative f, Gettable f, Integral n) => [n] -> Getting (Endo [a]) s s a a -> LensLike f s s [a] [a]
splittingPlaces s l f = coerce . traverse f . splitPlaces s . toListOf l
{-# INLINE splittingPlaces #-}

-- | Obtain a 'Fold' by splitting another 'Fold', 'Control.Lens.Type.Lens', 'Getter' or 'Control.Lens.Traversal.Traversal' into chunks of the given lengths.  Unlike 'splittingPlaces', the output 'Fold' will always be the same length as the first input argument.
--
-- @
-- 'splittingPlacesBlanks' :: 'Integral' n => [n] -> 'Fold' s a -> 'Fold' s [a]
-- @
splittingPlacesBlanks :: (Applicative f, Gettable f, Integral n) => [n] -> Getting (Endo [a]) s s a a -> LensLike f s s [a] [a]
splittingPlacesBlanks s l f = coerce . traverse f . splitPlacesBlanks s . toListOf l
{-# INLINE splittingPlacesBlanks #-}


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

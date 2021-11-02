{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
-------------------------------------------------------------------------------
module Numeric.Lens
  ( base
  , integral
    -- * Predefined bases
  , binary
  , octal
  , decimal
  , hex
    -- * Arithmetic lenses
  , adding
  , subtracting
  , multiplying
  , dividing
  , exponentiating
  , negated
  , pattern Integral
  ) where

import Control.Lens
import Data.CallStack
import Data.Char (chr, ord, isAsciiLower, isAsciiUpper, isDigit)
import Data.Maybe (fromMaybe)
import Numeric (readInt, showIntAtBase)

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import Data.Monoid (Sum(..))

-- | This 'Prism' can be used to model the fact that every 'Integral'
-- type is a subset of 'Integer'.
--
-- Embedding through the 'Prism' only succeeds if the 'Integer' would pass
-- through unmodified when re-extracted.
integral :: (Integral a, Integral b) => Prism Integer Integer a b
integral = prism toInteger $ \ i -> let a = fromInteger i in
  if toInteger a == i
  then Right a
  else Left i

pattern Integral :: Integral a => a -> Integer
pattern Integral a <- (preview integral -> Just a) where
  Integral a = review integral a

-- | A prism that shows and reads integers in base-2 through base-36
--
-- Note: This is an improper prism, since leading 0s are stripped when reading.
--
-- >>> "100" ^? base 16
-- Just 256
--
-- >>> 1767707668033969 ^. re (base 36)
-- "helloworld"
base :: (HasCallStack, Integral a) => Int -> Prism' String a
base b
  | b < 2 || b > 36 = error ("base: Invalid base " ++ show b)
  | otherwise       = prism intShow intRead
  where
    intShow n = showSigned' (showIntAtBase (toInteger b) intToDigit') (toInteger n) ""

    intRead s =
      case readSigned' (readInt (fromIntegral b) (isDigit' b) digitToInt') s of
        [(n,"")] -> Right n
        _ -> Left s
{-# INLINE base #-}

-- | Like 'Data.Char.intToDigit', but handles up to base-36
intToDigit' :: HasCallStack => Int -> Char
intToDigit' i
  | i >= 0  && i < 10 = chr (ord '0' + i)
  | i >= 10 && i < 36 = chr (ord 'a' + i - 10)
  | otherwise = error ("intToDigit': Invalid int " ++ show i)

-- | Like 'Data.Char.digitToInt', but handles up to base-36
digitToInt' :: HasCallStack => Char -> Int
digitToInt' c = fromMaybe (error ("digitToInt': Invalid digit " ++ show c))
                          (digitToIntMay c)

-- | A safe variant of 'digitToInt''
digitToIntMay :: Char -> Maybe Int
digitToIntMay c
  | isDigit c      = Just (ord c - ord '0')
  | isAsciiLower c = Just (ord c - ord 'a' + 10)
  | isAsciiUpper c = Just (ord c - ord 'A' + 10)
  | otherwise = Nothing

-- | Select digits that fall into the given base
isDigit' :: Int -> Char -> Bool
isDigit' b c = case digitToIntMay c of
  Just i -> i < b
  _ -> False

-- | A simpler variant of 'Numeric.showSigned' that only prepends a dash and
-- doesn't know about parentheses
showSigned' :: Real a => (a -> ShowS) -> a -> ShowS
showSigned' f n
  | n < 0     = showChar '-' . f (negate n)
  | otherwise = f n

-- | A simpler variant of 'Numeric.readSigned' that supports any base, only
-- recognizes an initial dash and doesn't know about parentheses
readSigned' :: Real a => ReadS a -> ReadS a
readSigned' f ('-':xs) = f xs <&> _1 %~ negate
readSigned' f xs       = f xs

-- | @'binary' = 'base' 2@
binary :: Integral a => Prism' String a
binary = base 2

-- | @'octal' = 'base' 8@
octal :: Integral a => Prism' String a
octal = base 8

-- | @'decimal' = 'base' 10@
decimal :: Integral a => Prism' String a
decimal = base 10

-- | @'hex' = 'base' 16@
hex :: Integral a => Prism' String a
hex = base 16

-- | @'adding' n = 'iso' (+n) (subtract n)@
--
-- >>> [1..3]^..traverse.adding 1000
-- [1001,1002,1003]
adding :: Num a => a -> Iso' a a
adding n = iso (+n) (subtract n)

-- | @
-- 'subtracting' n = 'iso' (subtract n) ((+n)
-- 'subtracting' n = 'from' ('adding' n)
-- @
subtracting :: Num a => a -> Iso' a a
subtracting n = iso (subtract n) (+n)

-- | @'multiplying' n = iso (*n) (/n)@
--
-- Note: This errors for n = 0
--
-- >>> 5 & multiplying 1000 +~ 3
-- 5.003
--
-- >>> let fahrenheit = multiplying (9/5).adding 32 in 230^.from fahrenheit
-- 110.0
multiplying :: (Fractional a, Eq a) => a -> Iso' a a
multiplying 0 = error "Numeric.Lens.multiplying: factor 0"
multiplying n = iso (*n) (/n)

-- | @
-- 'dividing' n = 'iso' (/n) (*n)
-- 'dividing' n = 'from' ('multiplying' n)@
--
-- Note: This errors for n = 0
dividing :: (Fractional a, Eq a) => a -> Iso' a a
dividing 0 = error "Numeric.Lens.dividing: divisor 0"
dividing n = iso (/n) (*n)

-- | @'exponentiating' n = 'iso' (**n) (**recip n)@
--
-- Note: This errors for n = 0
--
-- >>> au (_Wrapping Sum . from (exponentiating 2)) (foldMapOf each) (3,4) == 5
-- True
exponentiating :: (Floating a, Eq a) => a -> Iso' a a
exponentiating 0 = error "Numeric.Lens.exponentiating: exponent 0"
exponentiating n = iso (**n) (**recip n)


-- | @'negated' = 'iso' 'negate' 'negate'@
--
-- >>> au (_Wrapping Sum . negated) (foldMapOf each) (3,4) == 7
-- True
--
-- >>> au (_Wrapping Sum) (foldMapOf (each.negated)) (3,4) == -7
-- True
negated :: Num a => Iso' a a
negated = iso negate negate

{-# LANGUAGE Rank2Types #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Lens
-- Copyright   :  (C) 2012-13 Edward Kmett
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
  ) where

import Control.Lens
import Data.Char (chr, ord, isAsciiLower, isAsciiUpper, isDigit)
import Data.Maybe (fromMaybe)
import Numeric (readInt, showIntAtBase)

-- $setup
-- >>> :set -XNoOverloadedStrings

-- | This 'Prism' extracts can be used to model the fact that every 'Integral'
-- type is a subset of 'Integer'.
--
-- Embedding through the 'Prism' only succeeds if the 'Integer' would pass
-- through unmodified when re-extracted.
integral :: (Integral a, Integral b) => Prism Integer Integer a b
integral = prism toInteger $ \ i -> let a = fromInteger i in
  if toInteger a == i
  then Right a
  else Left i

-- | A prism that shows and reads integers in base-2 through base-36
--
-- >>> "100" ^? base 16
-- Just 256
--
-- >>> 1767707668033969 ^. re (base 36)
-- "helloworld"
base :: Integral a => Int -> Prism' String a
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
intToDigit' :: Int -> Char
intToDigit' i
  | i >= 0  && i < 10 = chr (ord '0' + i)
  | i >= 10 && i < 36 = chr (ord 'a' + i - 10)
  | otherwise = error ("intToDigit': Invalid int " ++ show i)

-- | Like 'Data.Char.digitToInt', but handles up to base-36
digitToInt' :: Char -> Int
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
readSigned' f ('-':xs) = f xs & mapped . _1 %~ negate
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

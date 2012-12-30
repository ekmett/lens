--------------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
-------------------------------------------------------------------------------
module Numeric.Lens (_int) where

import Control.Lens
import Data.Char (chr, ord, isAsciiLower, isAsciiUpper, isDigit)
import Data.Maybe (fromMaybe)
import Numeric (readInt, showIntAtBase)

-- | A prism that shows and reads integers in base-2 through base-36
--
-- >>> "100" ^? _int 16
-- Just 256
--
-- >>> 1767707668033969 ^. remit (_int 36)
-- "helloworld"
_int :: (Integral a, Show a) => a -> Prism' String a
_int base = validateBase `seq` prism intShow intRead
  where
    validateBase
      | base >= 2 && base <= 36 = ()
      | otherwise = error ("_int: Invalid base " ++ show base)

    intShow n = showSigned' (showIntAtBase base intToDigit') n ""

    intRead s =
      case readSigned' (readInt base (isDigit' base) digitToInt') s of
        [(n,"")] -> Right n
        _ -> Left s

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
isDigit' :: Integral a => a -> Char -> Bool
isDigit' base c = case digitToIntMay c of
  Just i | fromIntegral i < base -> True
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

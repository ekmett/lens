{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.Calendar.Lens
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  LiberalTypeSynonyms
--
-- Provides fairly ad hoc overloading to access different notions of a 'Day'.
--
-- To convert from a 'Day':
--
-- > myDay^.gregorian.year
-- > myDay^.julian.year
--
----------------------------------------------------------------------------
module Data.Time.Calendar.Lens
  ( modifiedJulianDay
  , TraverseDay(..)
  , HasYear(..)
  , HasMonth(..)
  , HasWeek(..)
  , HasDay(..)
  , Gregorian(..)
  , gregorian
  , Julian(..)
  , julian
  , WeekDate(..)
  , weekDate
  , OrdinalDate(..)
  , ordinalDate
  ) where

import Control.Applicative
import Control.Lens
import Data.Time.Calendar
import Data.Time.Calendar.Julian
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.OrdinalDate

class TraverseDay t where
  -- | Convert the type to a modified Julian day if possible and traverse it.
  --
  -- Traverses nothing if the date isn't valid.
  traverseDay :: Simple Traversal t Day

-- | Returns the modified Julian Day as a standard count of days,
-- with zero being the day 1858-11-17.
modifiedJulianDay :: Simple Lens Day Integer
modifiedJulianDay f = fmap ModifiedJulianDay . f . toModifiedJulianDay

instance TraverseDay Day where
  traverseDay = id

-- | Ad hoc overloading for accessing the year
class HasYear t where
  year :: Simple Lens t Integer

-- | Ad hoc overloading for accessing the month
class HasMonth t where
  month :: Simple Lens t Int

-- | Ad hoc overloading for accessing the week (what it is relative to may vary from type to type)
class HasWeek t where
  week :: Simple Lens t Int

-- | Ad hoc overloading for accessing the day (what it is relative to may vary from type to type)
class HasDay t where
  day :: Simple Lens t Int

-- | Date in the proleptic Gregorian calendar. First element of result is year, second month number (1-12), third day (1-31).
data Gregorian = Gregorian !Integer !Int !Int
  deriving (Eq,Ord,Show,Read)

-- | Convert to/from a valid date in the proleptic Gregorian calendar
gregorian :: Simple Lens Day Gregorian
gregorian f j = case toGregorian j of
  (y, m, d) -> (\(Gregorian y' m' d') -> fromGregorian y' m' d') <$> f (Gregorian y m d)

instance TraverseDay Gregorian where
  traverseDay f g@(Gregorian y m d) = case fromGregorianValid y m d of
    Nothing -> pure g
    Just j -> (\i -> case toGregorian i of (y', m', d') -> Gregorian y' m' d') <$> f j

instance HasYear Gregorian where
  year f (Gregorian y m d) = (\y' -> Gregorian y' m d) <$> f y

instance HasMonth Gregorian where
  month f (Gregorian y m d) = (\m' -> Gregorian y m' d) <$> f m

-- | Day of month
instance HasDay Gregorian where
  day f (Gregorian y m d) = Gregorian y m <$> f d

-- | Proleptic Julian year and day format.
-- First element of result is year (proleptic Julian calendar), second is the day of the year, with 1 for Jan 1, and 365 (or 366 in leap years) for Dec 31.
data Julian = Julian !Integer !Int
  deriving (Eq,Ord,Show,Read)

-- | Convert to/from a valid proleptic Julian year and day.
julian :: Simple Lens Day Julian
julian f j = case toJulianYearAndDay j of
  (y,d) -> go <$> f (Julian y d)
    where go (Julian y' d') = fromJulianYearAndDay y' d'

instance TraverseDay Julian where
  traverseDay f j@(Julian y d) = case fromJulianYearAndDayValid y d of
    Nothing -> pure j
    Just k -> (\i -> case toJulianYearAndDay i of (y', d') -> Julian y' d') <$> f k

instance HasYear Julian where
  year f (Julian y d) = (`Julian` d) <$> f y

-- | Day of year
instance HasDay Julian where
  day f (Julian y d) = Julian y <$> f d

-- | ISO 8601 Week Date format. First element of result is year, second week number (1-53), third day of week (1 for Monday to 7 for Sunday).
--
-- Note that "Week" years are not quite the same as Gregorian years, as the first day of the year is always a Monday.
--
-- The first week of a year is the first week to contain at least four days in the corresponding Gregorian year.
data WeekDate = WeekDate !Integer !Int !Int
  deriving (Eq,Ord,Show,Read)

-- | Convert to/from a valid WeekDate
weekDate :: Simple Lens Day WeekDate
weekDate f j = case toWeekDate j of
  (y,w,d) -> go <$> f (WeekDate y w d) where
    go (WeekDate y' w' d') = fromWeekDate y' w' d'

instance TraverseDay WeekDate where
  traverseDay f wd@(WeekDate y w d) = case fromWeekDateValid y w d of
    Nothing -> pure wd
    Just k -> (\i -> case toWeekDate i of (y', w', d') -> WeekDate y' w' d') <$> f k

instance HasYear WeekDate where
  year f (WeekDate y w d) = (\y' -> WeekDate y' w d) <$> f y

instance HasWeek WeekDate where
  week f (WeekDate y w d) = (\w' -> WeekDate y w' d) <$> f w

-- | Day of week
instance HasDay WeekDate where
  day f (WeekDate y w d) = WeekDate y w <$> f d

-- ISO 8601 Ordinal Date format. First element of result is year (proleptic Gregoran calendar), second is the day of the year, with 1 for Jan 1, and 365 (or 366 in leap years) for Dec 31.
data OrdinalDate = OrdinalDate !Integer Int

-- | Convert to/from a valid ISO 8601 Ordinal Date format.
ordinalDate :: Simple Lens Day OrdinalDate
ordinalDate f j = case toOrdinalDate j of
  (y, d) -> go <$> f (OrdinalDate y d) where
    go (OrdinalDate y' d') = fromOrdinalDate y' d'

instance TraverseDay OrdinalDate where
  traverseDay f od@(OrdinalDate y d) = case fromOrdinalDateValid y d of
    Nothing -> pure od
    Just k -> (\i -> case toOrdinalDate i of (y', d') -> OrdinalDate y' d') <$> f k

instance HasYear OrdinalDate where
  year f (OrdinalDate y d) = (`OrdinalDate` d) <$> f y

instance HasDay OrdinalDate where
  day f (OrdinalDate y d) = OrdinalDate y <$> f d

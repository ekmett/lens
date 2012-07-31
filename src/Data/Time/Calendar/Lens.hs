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
  , JulianYearAndDay(..)
  , julianYearAndDay
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

-- | Provide ad hoc overloading for traversing the modified Julian day
class TraverseDay t where
  -- | Convert the type to a modified Julian day if possible and traverse it.
  --
  -- Traverses nothing if the date isn't valid.
  traverseDay :: Simple Traversal t Day

-- | Returns the modified Julian Day as a standard count of days,
-- with zero being the day 1858-11-17.
modifiedJulianDay :: Simple Iso Day Integer
modifiedJulianDay = iso toModifiedJulianDay ModifiedJulianDay

instance TraverseDay Day where
  traverseDay = id

-- | Ad hoc overloading for accessing the year
class HasYear t where
  -- | Get the year of a date
  year :: Simple Lens t Integer

-- | Ad hoc overloading for accessing the month
class HasMonth t where
  -- | Get the month of a date
  month :: Simple Lens t Int

-- | Ad hoc overloading for accessing the week (what it is relative to may vary from type to type)
class HasWeek t where
  -- | Get the week of a date
  week :: Simple Lens t Int

-- | Ad hoc overloading for accessing the day (what it is relative to may vary from type to type)
class HasDay t where
  -- | Get the day of a date
  day :: Simple Lens t Int

-- | Date in the proleptic Gregorian calendar.
data Gregorian = Gregorian
  { gregorianYear :: !Integer -- ^ year
  , gregorianMonth :: !Int    -- ^ month (1-12)
  , gregorianDay :: !Int      -- ^ day   (1-31)
  } deriving (Eq,Ord,Show,Read)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

-- | Convert to/from a /valid/ date in the proleptic Gregorian calendar
gregorian :: Simple Iso Day Gregorian
gregorian = iso (uncurry3 Gregorian . toGregorian) $ \(Gregorian y m d) -> fromGregorian y m d

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
data JulianYearAndDay = JulianYearAndDay
  { julianYearAndDayYear :: !Integer -- ^ year (in the proleptic Julian calendar)
  , julianYearAndDayDay  :: !Int     -- ^ day of the year, with 1 for Jan 1, and 365 (or 366 in leap years) for Dec 31.
  } deriving (Eq,Ord,Show,Read)

-- | Convert to/from a /valid/ proleptic Julian year and day.
julianYearAndDay :: Simple Iso Day JulianYearAndDay
julianYearAndDay = iso (uncurry JulianYearAndDay . toJulianYearAndDay) $ \(JulianYearAndDay y d) -> fromJulianYearAndDay y d

instance TraverseDay JulianYearAndDay where
  traverseDay f j@(JulianYearAndDay y d) = case fromJulianYearAndDayValid y d of
    Nothing -> pure j
    Just k -> (\i -> case toJulianYearAndDay i of (y', d') -> JulianYearAndDay y' d') <$> f k

instance HasYear JulianYearAndDay where
  year f (JulianYearAndDay y d) = (`JulianYearAndDay` d) <$> f y

-- | Day of year
instance HasDay JulianYearAndDay where
  day f (JulianYearAndDay y d) = JulianYearAndDay y <$> f d

-- | ISO 8601 Week Date format.
--
-- The first week of a year is the first week to contain at least four days in the corresponding Gregorian year.
data WeekDate = WeekDate 
  { weekDateYear :: !Integer -- ^ year. Note: that "Week" years are not quite the same as Gregorian years, as the first day of the year is always a Monday.
  , weekDateWeek :: !Int -- ^ week number (1-53)
  , weekDateDay  :: !Int -- ^ day of week (1 for Monday to 7 for Sunday).
  } deriving (Eq,Ord,Show,Read)

-- | Convert to/from a valid WeekDate
weekDate :: Simple Iso Day WeekDate
weekDate = iso (uncurry3 WeekDate . toWeekDate) $ \(WeekDate y w d) -> fromWeekDate y w d

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

-- | ISO 8601 Ordinal Date format
data OrdinalDate = OrdinalDate
  { ordinalDateYear :: !Integer -- ^ year (proleptic Gregorian calendar)
  , ordinalDateDay  :: !Int     -- ^ day of the year, with 1 for Jan 1, and 365 (or 366 in leap years) for Dec 31.
  } deriving (Eq,Ord,Show,Read)

-- | Convert to/from a valid ISO 8601 Ordinal Date format.
ordinalDate :: Simple Iso Day OrdinalDate
ordinalDate = iso (uncurry OrdinalDate . toOrdinalDate) $ \(OrdinalDate y d) -> fromOrdinalDate y d

instance TraverseDay OrdinalDate where
  traverseDay f od@(OrdinalDate y d) = case fromOrdinalDateValid y d of
    Nothing -> pure od
    Just k -> (\i -> case toOrdinalDate i of (y', d') -> OrdinalDate y' d') <$> f k

instance HasYear OrdinalDate where
  year f (OrdinalDate y d) = (`OrdinalDate` d) <$> f y

instance HasDay OrdinalDate where
  day f (OrdinalDate y d) = OrdinalDate y <$> f d

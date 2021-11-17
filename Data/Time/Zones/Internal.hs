{- |
Module      : Data.Time.Zones.Internal
Copyright   : (C) 2014 Mihaly Barasz
License     : Apache-2.0, see LICENSE
Maintainer  : Mihaly Barasz <klao@nilcons.com>
Stability   : experimental
-}

{-# LANGUAGE CPP #-}
#ifdef TZ_TH
{-# LANGUAGE TemplateHaskell #-}
#endif

module Data.Time.Zones.Internal (
  -- * Time conversion to/from @Int64@
  utcTimeToInt64,
  utcTimeToInt64Pair,
  localTimeToInt64Pair,
  int64PairToUTCTime,
  int64PairToLocalTime,
  -- * POSIX-TZ helper functions
  ruleToSecs,
  yearToSecs,
  daysInMonth,
  monthToSecs,
  -- * Low-level \"coercions\"
  picoToInteger,
  integerToPico,
  diffTimeToPico,
  picoToDiffTime,
  diffTimeToInteger,
  integerToDiffTime,
  -- * Backwards combatibility
  getEnvMaybe,
  ) where

import Data.Bits ( Bits((.&.), shiftR) )
import Data.Fixed
import Data.Int
import Data.Time
import qualified Data.Vector.Unboxed as VU
import System.Environment ( getEnv )
import System.IO.Error ( catchIOError, isDoesNotExistError )
#ifdef TZ_TH
import Data.Time.Zones.Internal.CoerceTH
#else
import Unsafe.Coerce
#endif

import Data.Time.Zones.Types

utcTimeToInt64Pair :: UTCTime -> (Int64, Int64)
utcTimeToInt64Pair (UTCTime (ModifiedJulianDay d) t)
  = (86400 * (fromIntegral d - unixEpochDay) + s, ps)
  where
    (s, ps) = fromIntegral (diffTimeToInteger t) `divMod` 1000000000000
    unixEpochDay = 40587
{-# INLINE utcTimeToInt64Pair #-}

int64PairToLocalTime :: Int64 -> Int64 -> LocalTime
int64PairToLocalTime t ps = LocalTime (ModifiedJulianDay day) (TimeOfDay h m s)
  where
    (day64, tid64) = t `divMod` 86400
    day = fromIntegral $ day64 + 40587
    (h, ms) = fromIntegral tid64 `quotRem` 3600
    (m, s0) = ms `quotRem` 60
    s = integerToPico $ fromIntegral $ ps + 1000000000000 * fromIntegral s0
{-# INLINE int64PairToLocalTime #-}

localTimeToInt64Pair :: LocalTime -> (Int64, Int64)
localTimeToInt64Pair (LocalTime (ModifiedJulianDay day) (TimeOfDay h m s))
  = (86400 * (fromIntegral day - unixEpochDay) + tid, ps)
  where
    (s64, ps) = fromIntegral (picoToInteger s) `divMod` 1000000000000
    tid = s64 + fromIntegral (h * 3600 + m * 60)
    unixEpochDay = 40587
{-# INLINE localTimeToInt64Pair #-}

int64PairToUTCTime :: Int64 -> Int64 -> UTCTime
int64PairToUTCTime t ps = UTCTime (ModifiedJulianDay day) tid
  where
    (day64, tid64) = t `divMod` 86400
    day = fromIntegral $ day64 + 40587
    tid = integerToDiffTime $ fromIntegral $ ps + tid64 * 1000000000000
{-# INLINE int64PairToUTCTime #-}

utcTimeToInt64 :: UTCTime -> Int64
utcTimeToInt64 (UTCTime (ModifiedJulianDay d) t)
  = 86400 * (fromIntegral d - unixEpochDay)
    + fromIntegral (diffTimeToInteger t) `div` 1000000000000
  where
    unixEpochDay = 40587
{-# INLINE utcTimeToInt64 #-}

--------------------------------------------------------------------------------
-- POSIX-TZ helper functions

-- | Convert 'TzRule' plus year to number of seconds since epoch
--
-- See musl rule_to_secs()
ruleToSecs :: TzRule -> Int64 -> Int64
ruleToSecs (TzRule ty m n d t) y =
  ys + ms + fromIntegral t
  where
    secsperday = 86400
    isleap = isLeapYear (fromIntegral y)
    ys = yearToSecs y
    ms = case ty of
      TzRuleJ -> fromIntegral (if not isleap || d < 60 then d - 1 else d) * secsperday
      TzRuleN -> fromIntegral d * secsperday
      TzRuleM ->
        let
          -- s1 = seconds until start of the month
          s1 = fromIntegral $ monthToSecs isleap (m - 1)
          t0 = ys + s1
          wday = ((t0 + 4*secsperday) `mod` (7*secsperday)) `div` secsperday
          d1 = fromIntegral d - wday
          d2 = if d1 < 0 then d1 + 7 else d1
          n1 = fromIntegral $ if n == 5 && d2+28 >= fromIntegral (daysInMonth isleap m)
                              then 4
                              else n
          s2 = 86400 * (d2 + 7*(n1-1))
        in s1 + s2
{-# INLINE ruleToSecs #-}

-- | Number of seconds since epoch for year
yearToSecs :: Int64 -> Int64
yearToSecs y64 =
  utcTimeToInt64 $ UTCTime (fromGregorian (fromIntegral y64) 1 1) 0
{-# INLINE yearToSecs #-}

-- | Number of days in month
daysInMonth :: Bool -> Int -> Int
daysInMonth isleap 2 = 28 + if isleap then 1 else 0
daysInMonth _      m = 30 + ((0xad5 `shiftR` (m - 1)) .&. 1)
{-# INLINE daysInMonth #-}

-- | Number of seconds between start of year and end of month (1-12)
monthToSecs :: Bool -> Int -> Int
monthToSecs isleap m =
  d * 86400
  where
    d = VU.unsafeIndex sumdays m + if isleap && m > 2 then 1 else 0

    sumdays :: VU.Vector Int
    sumdays = VU.fromList [ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 ]
{-# INLINE monthToSecs #-}

--------------------------------------------------------------------------------
-- Low-level zero-overhead conversions.
-- Basically we could have used 'coerce' if the constructors were exported.

-- TODO(klao): Is it better to inline them saturated or unsaturated?

#ifdef TZ_TH

picoToInteger :: Pico -> Integer
picoToInteger p = $(destructNewType ''Fixed) p
{-# INLINE picoToInteger #-}

integerToPico :: Integer -> Pico
integerToPico i = $(constructNewType ''Fixed) i
{-# INLINE integerToPico #-}

diffTimeToPico :: DiffTime -> Pico
diffTimeToPico dt = $(destructNewType ''DiffTime) dt
{-# INLINE diffTimeToPico #-}

picoToDiffTime :: Pico -> DiffTime
picoToDiffTime p = $(constructNewType ''DiffTime) p
{-# INLINE picoToDiffTime #-}

diffTimeToInteger :: DiffTime -> Integer
diffTimeToInteger dt = picoToInteger (diffTimeToPico dt)
{-# INLINE diffTimeToInteger #-}

integerToDiffTime :: Integer -> DiffTime
integerToDiffTime i = picoToDiffTime (integerToPico i)
{-# INLINE integerToDiffTime #-}

#else

picoToInteger :: Pico -> Integer
picoToInteger = unsafeCoerce
{-# INLINE picoToInteger #-}

integerToPico :: Integer -> Pico
integerToPico = unsafeCoerce
{-# INLINE integerToPico #-}

diffTimeToPico :: DiffTime -> Pico
diffTimeToPico = unsafeCoerce
{-# INLINE diffTimeToPico #-}

picoToDiffTime :: Pico -> DiffTime
picoToDiffTime = unsafeCoerce
{-# INLINE picoToDiffTime #-}

diffTimeToInteger :: DiffTime -> Integer
diffTimeToInteger = unsafeCoerce
{-# INLINE diffTimeToInteger #-}

integerToDiffTime :: Integer -> DiffTime
integerToDiffTime = unsafeCoerce
{-# INLINE integerToDiffTime #-}

#endif

--------------------------------------------------------------------------------
-- Backwards compatibility

-- | This is equivalent to 'lookupEnv', defined for compatibility with
-- base < 4.6.0.0
getEnvMaybe :: String -> IO (Maybe String)
getEnvMaybe var =
  fmap Just (getEnv var) `catchIOError`
  (\e -> if isDoesNotExistError e then return Nothing else ioError e)

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
  -- * Low-level \"coercions\"
  picoToInteger,
  integerToPico,
  diffTimeToPico,
  picoToDiffTime,
  diffTimeToInteger,
  integerToDiffTime,
  ) where

import Data.Fixed
import Data.Int
import Data.Time
#ifdef TZ_TH
import Data.Time.Zones.Internal.CoerceTH
#else
import Unsafe.Coerce
#endif

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

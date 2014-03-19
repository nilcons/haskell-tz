{-# LANGUAGE BangPatterns #-}

module Data.Time.Zones (
  module Data.Time.Zones.Types,
  loadTZFromFile,
  diffForUTC,
  timeZoneForUTC,
  timeZoneForUTCTime,
  utcToLocalTimeTZ,
  ) where

import Data.Bits (shiftR)
import Data.Fixed (divMod')
import Data.Int (Int64)
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU
import Data.Time.Zones.Types
import Data.Time.Zones.Read (loadTZFromFile)

-- | Returns the time difference (in seconds) for TZ at the given
-- POSIX time.
diffForUTC :: TZ -> Int64 -> Int
{-# INLINE diffForUTC #-}
diffForUTC (TZ trans diffs _) t = VU.unsafeIndex diffs $ binarySearch trans t

-- | Returns the `TimeZone` for the `TZ` at the given POSIX time.
timeZoneForUTC :: TZ -> Int64 -> TimeZone
{-# INLINABLE timeZoneForUTC #-}
timeZoneForUTC (TZ trans diffs infos) t = TimeZone diffMins isDst name
  where
    i = binarySearch trans t
    diffMins = VU.unsafeIndex diffs i `div` 60
    (isDst, name) = VB.unsafeIndex infos i

-- | Returns the `TimeZone` for the `TZ` at the given `UTCTime`.
timeZoneForUTCTime :: TZ -> UTCTime -> TimeZone
{-# INLINABLE timeZoneForUTCTime #-}
timeZoneForUTCTime tz = timeZoneForUTC tz . floor . utcTimeToPOSIXSeconds

-- | Returns the `LocalTime` corresponding to the given `UTCTime` in `TZ`.
--
-- @utcToLocalTimeTZ tz ut@ is equivalent to @utcToLocalTime
-- (timeZoneForUTC tz ut) ut@ except when the time difference is not
-- an integral number of minutes
utcToLocalTimeTZ :: TZ -> UTCTime -> LocalTime
utcToLocalTimeTZ tz ut@(UTCTime day dtime) = LocalTime day' tod
  where
    diff = diffForUTC tz $ floor $ utcTimeToPOSIXSeconds ut
    (m', s) = (dtime + fromIntegral diff) `divMod'` 60
    (h', m) = m' `divMod` 60
    (d', h) = h' `divMod` 24
    day' = fromIntegral d' `addDays` day
    tod = TimeOfDay h m (realToFrac s)

-- | Returns the largest index `i` such that `v ! i <= t`.
--
-- Assumes that `v` is sorted, has at least one element and `v ! 0 <= t`.
binarySearch :: (VU.Unbox a, Ord a) => VU.Vector a -> a -> Int
{-# INLINE binarySearch #-}
binarySearch v t | n == 1    = 0
                 | otherwise = t `seq` go 1 n
  where
    n = VU.length v
    go !l !u | l >= u = (l - 1)
             | VU.unsafeIndex v k <= t  = go (k+1) u
             | otherwise  = go l k
      where
        k = (l + u) `shiftR` 1


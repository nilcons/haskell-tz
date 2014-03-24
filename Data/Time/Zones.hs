{-# LANGUAGE BangPatterns #-}

module Data.Time.Zones (
  module Data.Time.Zones.Types,
  loadTZFromFile,
  loadTZFromDB,
  loadSystemTZ,
  diffForUTC,
  timeZoneForUTC,
  timeZoneForUTCTime,
  utcToLocalTimeTZ,
  utcTZ,
  --
  LocalToUTCResult(..),
  localTimeToUTCFull,
  localTimeToUTCTZ,
  -- TODO(klao): do we want to export these?
  FromLocal(..),
  localToPOSIX,
  ) where

import Data.Bits (shiftR)
import Data.Fixed (divMod')
import Data.Int (Int64)
import Data.Time
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU
import Data.Time.Zones.Types
import Data.Time.Zones.Read

-- | Returns the time difference (in seconds) for TZ at the given
-- POSIX time.
diffForUTC :: TZ -> Int64 -> Int
{-# INLINE diffForUTC #-}
diffForUTC (TZ trans diffs _) t = VU.unsafeIndex diffs $ binarySearch trans t

timeZoneForIx :: TZ -> Int -> TimeZone
{-# INLINE timeZoneForIx #-}
timeZoneForIx (TZ _ diffs infos) i = TimeZone diffMins isDst name
  where
    diffMins = VU.unsafeIndex diffs i `div` 60
    (isDst, name) = VB.unsafeIndex infos i

-- | Returns the `TimeZone` for the `TZ` at the given POSIX time.
timeZoneForUTC :: TZ -> Int64 -> TimeZone
{-# INLINABLE timeZoneForUTC #-}
timeZoneForUTC tz@(TZ trans _ _) t = timeZoneForIx tz i
  where
    i = binarySearch trans t

-- | Returns the `TimeZone` for the `TZ` at the given `UTCTime`.
timeZoneForUTCTime :: TZ -> UTCTime -> TimeZone
{-# INLINABLE timeZoneForUTCTime #-}
timeZoneForUTCTime tz (UTCTime day tid)
  = timeZoneForUTC tz $ dayTimeToInt64 day tid

-- | Returns the `LocalTime` corresponding to the given `UTCTime` in `TZ`.
--
-- @utcToLocalTimeTZ tz ut@ is equivalent to @utcToLocalTime
-- (timeZoneForUTC tz ut) ut@ except when the time difference is not
-- an integral number of minutes
utcToLocalTimeTZ :: TZ -> UTCTime -> LocalTime
utcToLocalTimeTZ tz (UTCTime day dtime) = LocalTime day' tod
  where
    diff = diffForUTC tz $ dayTimeToInt64 day dtime
    (m', s) = (dtime + fromIntegral diff) `divMod'` 60
    (h', m) = m' `divMod` 60
    (d', h) = h' `divMod` 24
    day' = fromIntegral d' `addDays` day
    tod = TimeOfDay h m (realToFrac s)

-- | `TZ` structure for UTC.
utcTZ :: TZ
utcTZ = TZ (VU.singleton minBound) (VU.singleton 0) (VB.singleton (False, "UTC"))

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

--------------------------------------------------------------------------------
-- LocalTime to UTCTime

-- Representing LocalTimes as Int64s.
--
-- We want a simple and convenient mapping of
-- (year,month,day,hour,minutes,seconds) tuples onto a linear
-- scale. We adopt the following convention: an Int64 maps to the
-- tuple that a POSIX time represented by the same number would map in
-- UTC time zone. That is, 0 maps to '1970-01-01 00:00:00' and
-- 1395516860 maps to '2014-03-22 19:34:20'.
--
-- This is independent and without reference to any particular time
-- zone, it is completely analogous to LocalTime in that you need a
-- time zone to be able to interpret this as a point in time. And then
-- it may refer to an invalid time (spring time forward clock jump) or
-- two possible times (fall time backward jump).
--
-- So, it's basically another representation for LocalTimes (those
-- that can be represented within an Int64 and ignoring the fractional
-- seconds), except that LocalTime can contain invalid time-of-day
-- part (like 27:-05:107) and the Int64 representation can't and is
-- always unambiguous.
--
-- With that in mind, a UTCTime to LocalTime conversion can be seen as
-- 'TZ -> Int64 -> Int64', where the first Int64 is POSIX time and the
-- second is this kind of LocalTime representation.


-- | Internal representation of LocalTime -> UTCTime conversion result:
data FromLocal
  = FLGap    { _flIx :: {-# UNPACK #-} !Int
             , _flRes :: {-# UNPACK #-} !Int64 }
  | FLUnique { _flIx :: {-# UNPACK #-} !Int
             , _flRes :: {-# UNPACK #-} !Int64 }
  | FLDouble { _flIx :: {-# UNPACK #-} !Int
             , _flRes1 :: {-# UNPACK #-} !Int64
             , _flRes2 :: {-# UNPACK #-} !Int64 }
  deriving (Show)

-- We make the following two assumptions here:
--
-- 1. No diff in any time zone is ever bigger than 24 hours.
--
-- 2. No two consecutive transitions in any zone file ever fall within
--    48 hours of each other.
--
-- As a consequence, a local time might correspond to two different
-- points in time, but never three.
--
-- TODO(klao): check that these assuptions hold.
localToPOSIX :: TZ -> Int64 -> FromLocal
{-# INLINABLE localToPOSIX #-}
localToPOSIX (TZ trans diffs _) !lTime = res
  where
    lBound = lTime - 86400
    ix = binarySearch trans lBound
    cand1 = lTime - fromIntegral (VU.unsafeIndex diffs ix)
    res = if ix == VU.length trans
          then FLUnique ix cand1 -- TODO(klao): extend when rule handling is added
          else res'
               
    ix' = ix + 1
    nextTrans = VU.unsafeIndex trans ix'
    cand2 = lTime - fromIntegral (VU.unsafeIndex diffs ix')
    res' = case (cand1 < nextTrans, cand2 >= nextTrans) of
      (False, False) -> FLGap ix cand1
      (True,  False) -> FLUnique ix cand1
      (False, True)  -> FLUnique ix' cand2
      (True,  True)  -> FLDouble ix cand1 cand2

-- | Fully descriptive result of a LocalTime to UTCTime conversion.
--
-- In case of LTUAmbiguous the first result is always earlier than the
-- second one. Generally this only happens during the daylight saving
-- -> standard time transition (ie. summer -> winter). So, the first
-- result corresponds to interpreting the LocalTime as a daylight
-- saving time and the second result as standard time in the given
-- location.
--
-- But, if the location had some kind of administrative time
-- transition during which the clocks jumped back, then both results
-- can correspond to standard times (or daylight saving times) just
-- before and after the transition. You can always inspect the
-- 'timeZoneSummerOnly' field of the returned 'TimeZone's to get an
-- idea what kind of transition was taking place.
--
-- TODO(klao): document the LTUNone behavior.
data LocalToUTCResult
  = LTUNone { _ltuResult :: UTCTime
            , _ltuZone :: TimeZone }
  | LTUUnique { _ltuResult :: UTCTime
              , _ltuZone :: TimeZone }
  | LTUAmbiguous { _ltuFirst      :: UTCTime
                 , _ltuSecond     :: UTCTime
                 , _ltuFirstZone  :: TimeZone
                 , _ltuSecondZone :: TimeZone
                 } deriving (Eq, Show)

-- TODO(klao): measure the improvement
dayTimeToInt64 :: RealFrac a => Day -> a -> Int64
{-# INLINE dayTimeToInt64 #-}
dayTimeToInt64 (ModifiedJulianDay d) t = 86400 * (fromIntegral d - unixEpochDay) + floor t
  where
    unixEpochDay = 40587

-- TODO(klao): better name
localTimeToUTCFull :: TZ -> LocalTime -> LocalToUTCResult
localTimeToUTCFull tz@(TZ _ diffs _) (LocalTime day tod) = res
  where
    tid = timeOfDayToTime tod
    t = dayTimeToInt64 day tid
    addDiff i = UTCTime (addDays d day) tid'
      where
        diff = VU.unsafeIndex diffs i
        (d, tid') = (tid - fromIntegral diff) `divMod'` 86400
    res = case localToPOSIX tz t of
      FLGap i _ -> LTUNone (addDiff i) (timeZoneForIx tz i)
      FLUnique i _ -> LTUUnique (addDiff i) (timeZoneForIx tz i)
      FLDouble i _ _ -> LTUAmbiguous (addDiff i) (addDiff (i+1))
                          (timeZoneForIx tz i) (timeZoneForIx tz (i+1))

localTimeToUTCTZ :: TZ -> LocalTime -> UTCTime
localTimeToUTCTZ tz lt =
  case localTimeToUTCFull tz lt of
    LTUNone ut _ -> ut
    LTUUnique ut _ -> ut
    LTUAmbiguous _ ut _ _ -> ut

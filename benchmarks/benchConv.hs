module Main (main) where

import Criterion.Main
import Data.Int
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Zones.Internal

utc2int64Naive :: UTCTime -> Int64
utc2int64Naive ut = floor $ utcTimeToPOSIXSeconds ut

utc2int64Floor :: UTCTime -> Int64
utc2int64Floor (UTCTime (ModifiedJulianDay d) t)
  = 86400 * (fromIntegral d - unixEpochDay) + floor t
  where
    unixEpochDay = 40587

utc2int64Full :: UTCTime -> Int64
utc2int64Full (UTCTime (ModifiedJulianDay d) t)
  = 86400 * (fromIntegral d - unixEpochDay)
    + fromIntegral (diffTimeToInteger t) `div` 1000000000000
  where
    unixEpochDay = 40587


local2utcNaive :: LocalTime -> UTCTime
local2utcNaive (LocalTime day tod) = UTCTime day (timeOfDayToTime tod)

local2utcNaive2 :: LocalTime -> UTCTime
local2utcNaive2 = localTimeToUTC utc

local2utcFull :: LocalTime -> UTCTime
local2utcFull (LocalTime day (TimeOfDay h m s)) = UTCTime day tid
  where
    ps :: Int64
    ps = fromIntegral (picoToInteger s)
    tid = integerToDiffTime (fromIntegral tid64)
    tid64 = ps + fromIntegral h * 3600000000000000 + fromIntegral m * 60000000000000


utc2localNaive :: UTCTime -> LocalTime
utc2localNaive = utcToLocalTime utc

utc2localFull :: UTCTime -> LocalTime
utc2localFull (UTCTime day tid) = LocalTime day tod
  where
    tid64 :: Int64
    tid64 = fromIntegral (diffTimeToInteger tid)
    (hm, ps) = tid64 `divMod` 60000000000000
    (h, m) = fromIntegral hm `divMod` 60
    tod = TimeOfDay h m (integerToPico $ fromIntegral ps)


benchmarks :: [Benchmark]
benchmarks = [
  bgroup "UTCTimeToInt64" [
     bench "naive" $ whnf utc2int64Naive t,
     bench "floor" $ whnf utc2int64Floor t,
     bench "full"  $ whnf utc2int64Full  t
     ],
  bgroup "LocalToUTC" [
    bench "naive"  $ nf local2utcNaive  l,
    bench "naive2" $ nf local2utcNaive2 l,
    bench "full"   $ nf local2utcFull   l
    ],
  bgroup "UTCToLocal" [
    bench "naive" $ nf utc2localNaive t,
    bench "full"  $ nf utc2localFull t
    ]
  ]
  where
    t = UTCTime (fromGregorian 2014 07 23) 7268.123456789555
    l = LocalTime (fromGregorian 2014 07 23) (TimeOfDay 22 48 53.123456789888)

main :: IO ()
main = defaultMain benchmarks

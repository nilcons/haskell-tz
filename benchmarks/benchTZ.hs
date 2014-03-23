module Main (main) where

import Bindings.Posix.Time
import Criterion.Main
import Data.Fixed
import Data.Int
import Data.Time
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import Data.Time.Zones
import System.Posix.Env

setupTZ :: String -> IO TZ
setupTZ zoneName = do
  setEnv "TZ" zoneName True
  c'tzset
  loadSystemTZ zoneName

mkLocal :: Integer -> Int -> Int -> Int -> Int -> Pico -> LocalTime
mkLocal y m d hh mm ss
  = LocalTime (fromGregorian y m d) (TimeOfDay hh mm ss)

mkUTC :: Integer -> Int -> Int -> Int -> Int -> Pico -> UTCTime
mkUTC y m d hh mm ss
  = UTCTime (fromGregorian y m d) (timeOfDayToTime $ TimeOfDay hh mm ss)

utcToLocalNano :: TZ -> Int64 -> Int64
{-# INLINE utcToLocalNano #-}
utcToLocalNano tz t = t + 1000000000 * fromIntegral diff
  where
    diff = diffForUTC tz (t `div` 1000000000)

tzBenchmarks :: TZ -> TimeZoneSeries -> [Benchmark]
tzBenchmarks tz series = [
  bgroup "rawDiff" [
     bench "past" $ whnf (diffForUTC tz) (-10000000000), -- Way back in the past
     bench "epoch" $ whnf (diffForUTC tz) 0,
     bench "now" $ whnf (diffForUTC tz) 1395572400,
     bench "future" $ whnf (diffForUTC tz) 10000000000   -- Way in the future
     ],
  bgroup "utcToLocalNano" [
     bench "past" $ whnf (utcToLocalNano tz) (-4000000000000000000),
     bench "epoch" $ whnf (utcToLocalNano tz) 0,
     bench "now" $ whnf (utcToLocalNano tz) 1395572400000000000,
     bench "future" $ whnf (utcToLocalNano tz) 4000000000000000000
     ],
  bgroup "rawDiffUTC" [
     bench "now" $ whnf (diffForUTC utcTZ) 1395572400
     ],
  bgroup "basicUTCToLocalTime" [
    bench "past" $ nf (utcToLocalTime cetTZ) ut0,
    bench "now" $ nf (utcToLocalTime cetTZ) ut1,
    bench "future" $ nf (utcToLocalTime cetTZ) ut2
    ],
  bgroup "utcToLocalTimeTZ" [
    bench "past" $ nf (utcToLocalTimeTZ tz) ut0,
    bench "now" $ nf (utcToLocalTimeTZ tz) ut1,
    bench "future" $ nf (utcToLocalTimeTZ tz) ut2
    ],
  bgroup "utcToLocalTimeSeries" [
    bench "past" $ nf (utcToLocalTime' series) ut0,
    bench "now" $ nf (utcToLocalTime' series) ut1,
    bench "future" $ nf (utcToLocalTime' series) ut2
    ],
  bgroup "timeZoneForUTC" [
    bench "past" $ nf (timeZoneForUTC tz) (-10000000000),
    bench "now" $ nf (timeZoneForUTC tz) 1395572400,
    bench "future" $ nf (timeZoneForUTC tz) 10000000000
    ],
  bgroup "timeZoneForUTCTime" [
    bench "past" $ nf (timeZoneForUTCTime tz) ut0,
    bench "now" $ nf (timeZoneForUTCTime tz) ut1,
    bench "future" $ nf (timeZoneForUTCTime tz) ut2
    ],
  bgroup "timeZoneFromSeries" [
    bench "past" $ nf (timeZoneFromSeries series) ut0,
    bench "now" $ nf (timeZoneFromSeries series) ut1,
    bench "future" $ nf (timeZoneFromSeries series) ut2
    ],
  bgroup "localToPOSIX" [
    bench "past" $ whnf (localToPOSIX tz) (-10000000000),
    bench "now" $ whnf (localToPOSIX tz) 1396142115,
    bench "future" $ whnf (localToPOSIX tz) 10000000000
    ],
  bgroup "basicLocalTimeToUTC" [
    bench "past" $ nf (localTimeToUTC cetTZ) lt0,
    bench "now" $ nf (localTimeToUTC cetTZ) lt1,
    bench "future" $ nf (localTimeToUTC cetTZ) lt2
    ],
  bgroup "localTimeToUTCTZ" [
    bench "past" $ nf (localTimeToUTCTZ tz) lt0,
    bench "now" $ nf (localTimeToUTCTZ tz) lt1,
    bench "future" $ nf (localTimeToUTCTZ tz) lt2
    ],
  bgroup "localTimeToUTCSeries" [
    bench "past" $ nf (localTimeToUTC' series) lt0,
    bench "now" $ nf (localTimeToUTC' series) lt1,
    bench "future" $ nf (localTimeToUTC' series) lt2
    ]
  ]
  where
    cetTZ = TimeZone 60 False "CET"
    ut0 = mkUTC 1500 07 07  07 07 07
    ut1 = mkUTC 2014 03 23  15 15 15
    ut2 = mkUTC 2222 10 10  10 10 10
    lt0 = mkLocal 1500 07 07  07 07 07
    lt1 = mkLocal 2014 03 30  03 15 15
    lt2 = mkLocal 2222 10 10  10 10 10

main :: IO ()
main = do
  tzBudapest <- setupTZ "Europe/Budapest"
  seriesBudapest <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/Europe/Budapest"
  defaultMain $ tzBenchmarks tzBudapest seriesBudapest

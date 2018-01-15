{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import Data.Int (Int64)
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries(..))
import Data.Time.Zones
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit hiding (Test, assert)

case_utcTZ_is_utc = timeZoneForPOSIX utcTZ 0 @?= utc

case_utcTZ_zero_diff = diffForPOSIX utcTZ 0 @?= 0

case_DB_utc_is_utc = do
  tz <- loadTZFromDB "UTC"
  tz @?= utcTZ

mkLocal y m d hh mm ss
  = LocalTime (fromGregorian y m d) (TimeOfDay hh mm ss)

mkUTC y m d hh mm ss
  = UTCTime (fromGregorian y m d) (timeOfDayToTime $ TimeOfDay hh mm ss)

case_Budapest_LocalToUTC = do
  tz <- loadTZFromDB "Europe/Budapest"
  let zWinter = TimeZone 60 False "CET"
      zSummer = TimeZone 120 True "CEST"
  -- Handle std times:
  localTimeToUTCFull tz (mkLocal 1970 01 01  01 00 00) @?=
    LTUUnique (mkUTC 1970 01 01  00 00 00) zWinter
  localTimeToUTCFull tz (mkLocal 2014 03 23  00 15 15.15) @?=
    LTUUnique (mkUTC 2014 03 22  23 15 15.15) zWinter

  -- Handle time in winter->summer transition:
  localTimeToUTCFull tz (mkLocal 2014 03 30  02 15 15) @?=
    LTUNone (mkUTC 2014 03 30  01 15 15) zWinter
  -- That utc time is acually in dst already:
  localTimeToUTCFull tz (mkLocal 2014 03 30  03 15 15) @?=
    LTUUnique (mkUTC 2014 03 30  01 15 15) zSummer

  -- Handle dst times:
  localTimeToUTCFull tz (mkLocal 2014 04 05  06 07 08.987654321999) @?=
    LTUUnique (mkUTC 2014 04 05  04 07 08.987654321999) zSummer

  -- Handle time in summer->winter transition:
  localTimeToUTCFull tz (mkLocal 2013 10 27  02 15 15) @?=
    LTUAmbiguous (mkUTC 2013 10 27  00 15 15) (mkUTC 2013 10 27  01 15 15)
      zSummer zWinter

-- Local->UTC, test for time zones that stop having DST.
-- Bug reported in #8
case_Moscow_LocalToUTC = do
  tz <- loadTZFromDB "Europe/Moscow"
  let zMoscow = TimeZone 180 False "MSK"
  localTimeToUTCFull tz (mkLocal 2015 11 23  00 00 00) @?=
    LTUUnique (mkUTC 2015 11 22  21 00 00) zMoscow

case_UTC_diffForAbbr = do
  tz <- loadTZFromDB "UTC"
  diffForAbbr tz "UTC" @?= Just 0
  diffForAbbr tz "XYZ" @?= Nothing

case_Paris_diffForAbbr = do
  tz <- loadTZFromDB "Europe/Paris"
  diffForAbbr tz "CET" @?= Just 3600
  diffForAbbr tz "CEST" @?= Just 7200
  diffForAbbr tz "WET" @?= Just 0
  diffForAbbr tz "LMT" @?= Just 561
  diffForAbbr tz "XYZ" @?= Nothing

posixSecondsToUTCTime' :: Int64 -> UTCTime
posixSecondsToUTCTime' = posixSecondsToUTCTime . fromIntegral

case_UTC_toTimeZoneSeries = do
  tz <- loadTZFromDB "UTC"
  -- has only nomial series
  toTimeZoneSeries tz @?= Just
    TimeZoneSeries {
      tzsTimeZone = utc
    , tzsTransitions = [(posixSecondsToUTCTime' minBound, utc)]
    }

case_Pacific_toBoundedTimeZoneSeries = do
  tz <- loadTZFromDB "America/Los_Angeles"
  let yStart = localTimeToUTCTZ tz (mkLocal 2017 01 01  00 00 00)
      dStart = localTimeToUTCTZ tz (mkLocal 2017 03 12  03 00 00)
      dAny   = localTimeToUTCTZ tz (mkLocal 2017 08 15  00 00 00)
      dEnd   = localTimeToUTCTZ tz (mkLocal 2017 11 05  00 59 59)
      sAny   = localTimeToUTCTZ tz (mkLocal 2017 12 15  00 00 00)
      yEnd   = localTimeToUTCTZ tz (mkLocal 2017 12 31  23 59 59)
      pst = TimeZone (-480) False "PST"
      pdt = TimeZone (-420) True "PDT"

  -- daylight period has no transitions
  toBoundedTimeZoneSeries tz dStart dEnd @?= Nothing
  toBoundedTimeZoneSeries tz dEnd dStart @?= Nothing

  -- daylight to standard has one transition
  let seriesDToS = Just
        TimeZoneSeries {
          tzsTimeZone = pst
        , tzsTransitions = [(posixSecondsToUTCTime' 1509872400, pst)]
        }
  toBoundedTimeZoneSeries tz dAny sAny @?= seriesDToS
  toBoundedTimeZoneSeries tz sAny dStart @?= seriesDToS

  -- a pacific year has two transitions
  let seriesYear = Just
        TimeZoneSeries {
          tzsTimeZone = pst
        , tzsTransitions = [
              (posixSecondsToUTCTime' 1509872400, pst)
            , (posixSecondsToUTCTime' 1489312800, pdt)
            ]
        }
  toBoundedTimeZoneSeries tz yStart yEnd @?= seriesYear
  toBoundedTimeZoneSeries tz yEnd yStart @?= seriesYear

main :: IO ()
main = $defaultMainGenerator

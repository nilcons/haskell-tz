{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import Bindings.Posix.Time
import Data.Bits
import Data.Int
import Data.IORef
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Zones
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.HUnit hiding (Test, assert)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Posix.Env
import System.IO.Unsafe

setupTZ :: String -> IO TZ
setupTZ zoneName = do
  setEnv "TZ" zoneName True
  c'tzset
  loadSystemTZ zoneName

onceIO :: IO a -> IO a
{-# NOINLINE onceIO #-}
onceIO op = opWrap
  where
    {-# NOINLINE var #-}
    var = unsafePerformIO $ newIORef Nothing
    opWrap = do
      v <- readIORef var
      case v of
        Just x -> return x
        Nothing -> do
          x <- op
          atomicWriteIORef var $ Just x
          return x

-- On the Int32 range of POSIX times we should replicate the behavior
-- perfectly.
--
-- * After year 2038 we normally run into a range where the
-- envvar-like "rule" part of the TZif should be interpreted, which we
-- don't do yet.
--
-- * And below around -2^55 the localtime_r C function starts failing
-- with "value too large".
checkTimeZone :: String -> Int32 -> Property
checkTimeZone zoneName = prop
  where
    setup = onceIO $ setupTZ zoneName
    prop ut = monadicIO $ do
      tz <- run $ setup
      run $ print ut
      timeZone <- run $ getTimeZone $ posixSecondsToUTCTime $ fromIntegral ut
      run $ timeZoneForUTC tz (fromIntegral ut) @?= timeZone

-- See comment for the checkTimeZone.
checkTimeZone64 :: String -> Property
checkTimeZone64 zoneName = prop
  where
    setup = onceIO $ setupTZ zoneName
    two31 = 2147483647
    prop = monadicIO $ do
      tz <- run $ setup
      ut <- pick $ oneof [arbitrary, choose (-two31, two31)]
      pre $ ut < two31 && ut > -(1 `shiftL` 55)
      -- This is important. On 32 bit machines we want to limit
      -- testing to the Int range.
      pre $ ut > fromIntegral (minBound :: Int)
      timeZone <- run $ getTimeZone $ posixSecondsToUTCTime $ fromIntegral ut
      run $ timeZoneForUTC tz ut @?= timeZone

-- On the Int32 range of POSIX times we should mostly replicate the
-- behavior.
--
-- * After year 2038 we normally run into a range where the
-- envvar-like "rule" part of the TZif should be interpreted, which we
-- don't do yet.
--
-- * And the very first time diff in most of the TZif files is usually
-- the "Local Mean Time", which is generally a fractional number of
-- minutes, so we would get difference with getTimeZone too. Most of
-- the locations switch to some more standard time zone before or
-- around 1900, which happens to be less than -2^31 POSIX time.  But
-- in some locations this transition falls within the Int32 range
-- (eg. China), so we can supply another lower bound.
checkLocalTime :: String -> Maybe Int32 -> Int32 -> Property
checkLocalTime zoneName mLower = prop
  where
    setup = onceIO $ setupTZ zoneName
    prop ut = monadicIO $ do
      case mLower of
        Nothing -> return ()
        Just lower -> pre $ ut > lower
      tz <- run $ setup
      let utcTime = posixSecondsToUTCTime $ fromIntegral ut
      timeZone <- run $ getTimeZone utcTime
      run $ utcToLocalTimeTZ tz utcTime @?= utcToLocalTime timeZone utcTime


case_utcTZ_is_utc = timeZoneForUTC utcTZ 0 @?= utc

case_utcTZ_zero_diff = diffForUTC utcTZ 0 @?= 0

prop_Budapest_correct_TimeZone = checkTimeZone64 "Europe/Budapest"
prop_New_York_correct_TimeZone = checkTimeZone64 "America/New_York"
prop_Los_Angeles_correct_TimeZone = checkTimeZone64 "America/Los_Angeles"
prop_Shanghai_correct_TimeZone = checkTimeZone64 "Asia/Shanghai"
prop_Jerusalem_correct_TimeZone = checkTimeZone64 "Asia/Jerusalem"
prop_Antarctica_Palmer_correct_TimeZone = checkTimeZone64 "Antarctica/Palmer"
prop_Melbourne_correct_TimeZone = checkTimeZone64 "Australia/Melbourne"

prop_Budapest_correct_LocalTime = checkLocalTime "Europe/Budapest" Nothing
prop_New_York_correct_LocalTime = checkLocalTime "America/New_York" Nothing
prop_Los_Angeles_correct_LocalTime = checkLocalTime "America/Los_Angeles" Nothing
prop_Shanghai_correct_LocalTime = checkLocalTime "Asia/Shanghai" $ Just (-1325491558)
prop_Jerusalem_correct_LocalTime = checkLocalTime "Asia/Jerusalem" $ Just (-1641003641)
prop_Antarctica_Palmer_correct_LocalTime = checkLocalTime "Antarctica/Palmer" Nothing
prop_Melbourne_correct_LocalTime = checkLocalTime "Australia/Melbourne" Nothing

case_DB_utc_is_utc = do
  tz <- loadTZFromDB "UTC"
  tz @?= utcTZ

mkLocal y m d hh mm ss
  = LocalTime (fromGregorian y m d) (TimeOfDay hh mm ss)

mkUTC y m d hh mm ss
  = UTCTime (fromGregorian y m d) (timeOfDayToTime $ TimeOfDay hh mm ss)

case_Budapest_LocalToUTC = do
  tz <- loadTZFromDB "Europe/Budapest"
  let zstd = TimeZone 60 False "CET"
      zdst = TimeZone 120 True "CEST"
  localTimeToUTCFull tz (mkLocal 1970 01 01  01 00 00) @?=
    LTUUnique (mkUTC 1970 01 01  00 00 00) zstd
  localTimeToUTCFull tz (mkLocal 2014 03 23  00 15 15.15) @?=
    LTUUnique (mkUTC 2014 03 22  23 15 15.15) zstd
  localTimeToUTCFull tz (mkLocal 2014 03 30  02 15 15) @?= LTUNone
  localTimeToUTCFull tz (mkLocal 2014 04 05  06 07 08.987654321999) @?=
    LTUUnique (mkUTC 2014 04 05  04 07 08.987654321999) zdst
  localTimeToUTCFull tz (mkLocal 2013 10 27  02 15 15) @?=
    LTUAmbiguous (mkUTC 2013 10 27  00 15 15) (mkUTC 2013 10 27  01 15 15) zdst zstd

main :: IO ()
main = do
  -- When we are running 'cabal test' the package is not yet
  -- installed, so we want to use the data directory from within the
  -- sources.
  setEnv "tz_datadir" "./tzdata" True
  $defaultMainGenerator

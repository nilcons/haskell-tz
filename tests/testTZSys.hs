{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import Data.Bits
import Data.IORef
import Data.Int
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Zones
import Data.Time.Zones.Read (pathForSystemTZ)
import System.Environment
import System.IO.Unsafe
import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

foreign import ccall safe "time.h tzset" c_tzset :: IO ()

setupTZ :: String -> IO TZ
setupTZ zoneName = do
  -- tzset(3) doesn't necessarily understand TZDIR, since it is
  -- not mandated by POSIX. To avoid trouble with non-glibc
  -- libc implementations (musl, libSystem, ...) we set TZ
  -- to an absolute path (with a leading ':') which all
  -- POSIX-conforming implementations must support.
  tzPath <- pathForSystemTZ zoneName
  setEnv "TZ" $ ":" ++ tzPath
  c_tzset
  loadSystemTZ zoneName

onceIO :: IO a -> IO a
{-# NOINLINE onceIO #-}
onceIO op = opWrap
  where
    {-# NOINLINE var #-}
    var = unsafePerformIO $ newIORef $ Left op
    opWrap = do
      v <- readIORef var
      case v of
        Right x -> return x
        Left _ -> do
          x <- op
          atomicWriteIORef var $ Right x
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
      run $ timeZoneForPOSIX tz ut @?= timeZone

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

main :: IO ()
main = $defaultMainGenerator

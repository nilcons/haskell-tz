{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import Data.IORef
import Data.Int
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Zones
import Data.Time.Zones.Read
import Data.Time.Zones.Types
import System.Environment
import System.FilePath.Find
import System.IO.Unsafe
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.HUnit hiding (Test, assert)
import Test.QuickCheck
import Test.QuickCheck.Monadic

foreign import ccall safe "time.h tzset" c_tzset :: IO ()

setupTZ :: String -> IO TZ
setupTZ zoneName = do
  setEnv "TZ" zoneName
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
-- * After year 2038 we run into a range where the envvar-like "rule"
-- part of the TZif is interpreted. However, above around 2^47
-- glibc starts producing wrong results (tested on a system with
-- version 2.32):
--
-- % env TZ=America/Los_Angeles date -d '5881580-7-1 00:00 UTC'
-- Mon Jun 30 05:00:00 PM PDT 5881580
-- % env TZ=America/Los_Angeles date -d '5881581-7-1 00:00 UTC'
-- Tue Jun 30 04:00:00 PM PST 5881581
--
-- > logBase 2 $ fromIntegral $ utcTimeToInt64 $ UTCTime (fromGregorian 5881581 7 1) 0
-- 47.398743929757934
--
-- * And below around -2^55 the localtime_r C function starts failing
-- with "value too large".
checkTimeZone64 :: String -> Property
checkTimeZone64 zoneName = withMaxSuccess 1000 prop
  where
    setup = onceIO $ setupTZ zoneName
    two31 = 2^(31 :: Int) - 1
    two47 = 2^(47 :: Int)
    two55 = 2^(55 :: Int)
    prop = monadicIO $ do
      tz <- run setup
      ut <- pick $ oneof [arbitrary, choose (-two31, two31)]
      pre $ ut < two47 && ut > -two55
      -- This is important. On 32 bit machines we want to limit
      -- testing to the Int range.
      pre $ ut > fromIntegral (minBound :: Int)
      timeZone <- run $ getTimeZone $ posixSecondsToUTCTime $ fromIntegral ut
      run $ timeZoneForPOSIX tz ut @?= timeZone

-- On the Int32 range of POSIX times we should mostly replicate the
-- behavior.
--
-- * After year 2038 we run into a range where the envvar-like "rule"
-- part of the TZif is interpreted but above around 2^47 glibc starts
-- returning wrong results.
--
-- * And the very first time diff in most of the TZif files is usually
-- the "Local Mean Time", which is generally a fractional number of
-- minutes, so we would get difference with getTimeZone too. Most of
-- the locations switch to some more standard time zone before or
-- around 1900, which happens to be less than -2^31 POSIX time.  But
-- in some locations this transition falls within the Int32 range
-- (eg. China), so we can supply another lower bound.
--
-- Warning: seems to leak memory, be careful running for more than
-- 100k successes.
checkLocalTime :: String -> Maybe Int32 -> Int64 -> Property
checkLocalTime zoneName mLower = withMaxSuccess 1000 . prop
  where
    two31 = 2^(31 :: Int)
    two47 = 2^(47 :: Int)
    setup = onceIO $ setupTZ zoneName
    prop ut = monadicIO $ do
      pre $ ut < two47
      case mLower of
        Nothing -> pre $ ut > -two31
        Just lower -> pre $ ut > fromIntegral lower
      tz <- run setup
      let utcTime = posixSecondsToUTCTime $ fromIntegral ut
      timeZone <- run $ getTimeZone utcTime
      run $ utcToLocalTimeTZ tz utcTime @?= utcToLocalTime timeZone utcTime

-- | Check if all zone files in system directory parse
--
-- This is useful to verify parsing but overkill to run on every
-- test or install so first parameter says if we want to run or not.
--
-- This can fail if any of the files in listed dirs is in v1 format
-- which does not have POSIX-TZ strings.
checkAllParse ::  IO ()
checkAllParse = do
  tzdir <- pathForSystemTZ ""
  for_ dirs $ \dir -> do
    let curdir = tzdir <> "/" <> dir
    xs <- find always (fileType ==? RegularFile) curdir
    for_ xs $ \p -> do
      tz <- loadTZFromFile p
      assertBool ("missing POSIX-TZ (v1 file) or parsing failure: " <> p) (isJust $ _tzPosixTz tz)
  where
    dirs = [ "Africa", "America", "Antarctica", "Arctic"
           , "Asia", "Atlantic", "Australia", "Brazil"
           , "Canada", "Chile", "Etc", "Europe", "Indian"
           , "Mexico", "Pacific", "US"
           ]

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

-- disabled by default
-- case_All_parse = checkAllParse True

main :: IO ()
main = $defaultMainGenerator

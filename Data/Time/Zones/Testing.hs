{-# LANGUAGE ViewPatterns #-}

module Data.Time.Zones.Testing (
  seriesToTZ,
  loadViaSeriesTZ,
  ) where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as VB
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import Data.Time.Zones.Types

seriesToTZ :: TimeZoneSeries -> TZ
seriesToTZ (TimeZoneSeries defaultTZ (reverse -> series)) = TZ trans diffs infos
  where
    trans = VU.fromList $ minBound : map (utcToInt64 . fst) series
    tzList = defaultTZ : map snd series
    diffs = VU.fromList $ map tzToInt tzList
    infos = VB.fromList $ map tzToInfo tzList
    utcToInt64 = floor . utcTimeToPOSIXSeconds
    tzToInt = fromIntegral . (60*) . timeZoneMinutes
    tzToInfo (TimeZone _ summerOnly name) = (summerOnly, name)

loadViaSeriesTZ :: FilePath -> IO TZ
loadViaSeriesTZ = fmap seriesToTZ . getTimeZoneSeriesFromOlsonFile

{- |
Module      : Data.Time.Zones.Read
Copyright   : (C) 2014 Mihaly Barasz
License     : Apache-2.0, see LICENSE
Maintainer  : Mihaly Barasz <klao@nilcons.com>
Stability   : experimental
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Time.Zones.Read (
  -- * Various ways of loading `TZ`
  loadTZFromFile,
  loadSystemTZ,
  pathForSystemTZ,
  loadLocalTZ,
  loadTZFromDB,
  -- * Parsing Olson data
  olsonGet,
  parseOlson,
  ) where

import Control.Applicative
import Control.Exception (assert)
import Control.Monad
import Data.Binary
import Data.Binary.Get (getByteString, getWord32be, getWord64be, runGet, skip)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as VB
import Data.Int
import Data.Time.Zones.Files
import Data.Time.Zones.Types
import System.Environment
import System.IO.Error

-- Suppress 'redundant imports' warning
import Prelude

-- | Reads and parses a time zone information file (in @tzfile(5)@
-- aka. Olson file format) and returns the corresponding TZ data
-- structure.
loadTZFromFile :: FilePath -> IO TZ
loadTZFromFile fname = runGet olsonGet <$> BL.readFile fname

-- | Looks for the time zone file in the system timezone directory, which is
-- @\/usr\/share\/zoneinfo@, or if the @TZDIR@ environment variable is
-- set, then there.
loadSystemTZ :: String -> IO TZ
loadSystemTZ tzName = pathForSystemTZ tzName >>= loadTZFromFile

-- | Return the path for a time zone file in the system time zone directory.
--
-- The system directory is specified by the @TZDIR@ environment variable,
-- or @\/usr\/share\/zoneinfo@ if it's not set.
pathForSystemTZ :: String -> IO FilePath
pathForSystemTZ tzName = do
  dir <- fromMaybe "/usr/share/zoneinfo" <$> getEnvMaybe "TZDIR"
  return $ dir ++ "/" ++ tzName

-- | Returns the local `TZ` based on the @TZ@ and @TZDIR@
-- environment variables.
--
-- See @tzset(3)@ for details, but basically:
--
-- * If @TZ@ environment variable is unset, we @loadTZFromFile \"\/etc\/localtime\"@.
--
-- * If @TZ@ is set, but empty, we @loadSystemTZ \"UTC\"@.
--
-- * Otherwise, we just @loadSystemTZ@ it.
--
-- Note, this means we don't support POSIX-style @TZ@ variables (like
-- @\"EST5EDT\"@), only those that are explicitly present in the time
-- zone database.
loadLocalTZ :: IO TZ
loadLocalTZ = do
  tzEnv <- getEnvMaybe "TZ"
  case tzEnv of
    Nothing -> loadTZFromFile "/etc/localtime"
    Just "" -> loadSystemTZ "UTC"
    Just z -> loadSystemTZ z

getEnvMaybe :: String -> IO (Maybe String)
getEnvMaybe var =
  fmap Just (getEnv var) `catchIOError`
  (\e -> if isDoesNotExistError e then return Nothing else ioError e)

-- | Reads the corresponding file from the time zone database shipped
-- with this package.
loadTZFromDB :: String -> IO TZ
loadTZFromDB tzName = do
  fn <- timeZonePathFromDB tzName
  loadTZFromFile fn

--------------------------------------------------------------------------------

olsonGet :: Get TZ
olsonGet = do
  version <- olsonHeader
  case () of
    () | version == '\0' -> olsonGetWith 4 getTime32
    () | version `elem` ['2', '3'] -> do
      skipOlson0 >> void olsonHeader
      olsonGetWith 8 getTime64
      -- TODO(klao): read the rule string
    _ -> fail $ "olsonGet: invalid version character: " ++ show version

parseOlson :: BL.ByteString -> TZ
parseOlson = runGet olsonGet

olsonHeader :: Get Char
olsonHeader = do
  magic <- getByteString 4
  unless (magic == "TZif") $ fail "olsonHeader: bad magic"
  version <- toEnum <$> getInt8
  skip 15
  return version

skipOlson0 :: Get ()
skipOlson0 = do
  tzh_ttisgmtcnt <- getInt32
  tzh_ttisstdcnt <- getInt32
  tzh_leapcnt <- getInt32
  tzh_timecnt <- getInt32
  tzh_typecnt <- getInt32
  tzh_charcnt <- getInt32
  skip $ (4 * tzh_timecnt) + tzh_timecnt + (6 * tzh_typecnt) + tzh_charcnt +
    (8 * tzh_leapcnt) + tzh_ttisstdcnt + tzh_ttisgmtcnt

olsonGetWith :: Int -> Get Int64 -> Get TZ
olsonGetWith szTime getTime = do
  tzh_ttisgmtcnt <- getInt32
  tzh_ttisstdcnt <- getInt32
  tzh_leapcnt <- getInt32
  tzh_timecnt <- getInt32
  tzh_typecnt <- getInt32
  tzh_charcnt <- getInt32
  transitions <- VU.replicateM tzh_timecnt getTime
  indices <- VU.replicateM tzh_timecnt getInt8
  infos <- VU.replicateM tzh_typecnt getTTInfo
  abbrs <- getByteString tzh_charcnt
  skip $ tzh_leapcnt * (szTime + 4)
  skip tzh_ttisstdcnt
  skip tzh_ttisgmtcnt
  let isDst (_,x,_) = x
      gmtOff (x,_,_) = x
      isDstName (_,d,ni) = (d, abbrForInd ni abbrs)
      vInfos = VU.map (infos VU.!) indices
      -- Older tz databases didn't have an explicit first transition,
      -- but since 2014c they do.  (In 2014c it's minBound and in
      -- later versions it's the large negative constant below.)
      (eTransitions, eInfos) = case () of
        _ | hasInitTrans -> assert infosHeadIsDst (transitions', vInfos)
        _ -> (VU.cons minBound transitions, VU.cons first vInfos)
        where
          hasInitTrans = not (VU.null transitions)
                         && VU.head transitions <= -0x800000000000000
          transitions' = transitions VU.// [(0, minBound)]
          infosHeadIsDst = not $ isDst $ VU.head infos
          lInfos = VU.toList infos
          first = head $ filter (not . isDst) lInfos ++ lInfos
      diffs = VU.map gmtOff eInfos
      tzInfos = VB.fromListN (VU.length eInfos) $ map isDstName $ VU.toList eInfos
  return $ TZ eTransitions diffs tzInfos

abbrForInd :: Int -> BS.ByteString -> String
abbrForInd i = BS.unpack . BS.takeWhile (/= '\0') . BS.drop i

getTTInfo :: Get (Int, Bool, Int)  -- (gmtoff, isdst, abbrind)
getTTInfo = (,,) <$> getInt32 <*> get <*> getInt8

getInt8 :: Get Int
{-# INLINE getInt8 #-}
getInt8 = fromIntegral <$> getWord8

getInt32 :: Get Int
{-# INLINE getInt32 #-}
getInt32 = (fromIntegral :: Int32 -> Int) . fromIntegral <$> getWord32be

getTime32 :: Get Int64
{-# INLINE getTime32 #-}
getTime32 = fromIntegral <$> getInt32

getTime64 :: Get Int64
{-# INLINE getTime64 #-}
getTime64 = fromIntegral <$> getWord64be

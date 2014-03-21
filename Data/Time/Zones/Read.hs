{-# LANGUAGE OverloadedStrings #-}

module Data.Time.Zones.Read (
  loadTZFromFile,
  loadSystemTZ,
  loadTZFromDB,
  olsonGet,
  OlsonInfo(..),
  loadOlsonInfo,
  ) where

import Control.Applicative
import Control.Monad (unless, replicateM)
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Vector.Generic (stream, unstream)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as VB
import Data.Int
import Data.Time.Zones.Types
import System.Environment
import System.IO.Error

import Paths_tz hiding (version)

-- | Reads and parses a time zone information file (in tzfile(5)
-- aka. Olson file format) and returns the corresponding TZ data
-- structure.
loadTZFromFile :: FilePath -> IO TZ
loadTZFromFile fname = runGet olsonGet <$> BL.readFile fname

-- | Looks for the time zone file in standard directory, which is
-- @/usr/share/zoneinfo@ or if the @TZDIR@ environment variable is
-- set, then there.
loadSystemTZ :: String -> IO TZ
loadSystemTZ tzName = do
  dir <- getEnvDefault "TZDIR" "/usr/share/zoneinfo"
  loadTZFromFile $ dir ++ "/" ++ tzName

getEnvDefault :: String -> String -> IO String
getEnvDefault var fallback =
  getEnv var `catchIOError`
  (\e -> if isDoesNotExistError e then return fallback else ioError e)

-- | Reads the corresponding file from the time zone database shipped
-- with this package.
loadTZFromDB :: String -> IO TZ
loadTZFromDB tzName = do
  -- TODO(klao): this probably won't work on Windows.
  fn <- getDataFileName $ tzName ++ ".zone"
  loadTZFromFile fn


olsonGet :: Get TZ
olsonGet = do
  version <- olsonHeader
  case () of
    () | version == '\0' -> olsonGetWith 4 getTime32
    -- ()  -> olsonGetWith 4 getTime32
    () | version `elem` ['2', '3'] -> do
      skipOlson0
      _ <- olsonHeader
      olsonGetWith 8 getTime64
      -- TODO(klao): read the rule string
    _ -> fail $ "olsonGet: invalid version character: " ++ show version


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
      lInfos = VU.toList infos
      first = head $ filter (not . isDst) lInfos ++ lInfos
      vtrans = VU.cons minBound transitions
      eInfos = VU.cons first $ VU.map (infos VU.!) indices
      vdiffs = VU.map gmtOff eInfos
      vinfos = VB.map isDstName $ unstream $ stream eInfos
  return $ TZ vtrans vdiffs vinfos

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

--------------------------------------------------------------------------------
-- Debugging:

loadOlsonInfo :: FilePath -> IO OlsonInfo
loadOlsonInfo fname = runGet olsonInfoGet <$> BL.readFile fname


data OlsonInfo = OlsonInfo {
  transCnt :: Int,
  infosCnt :: Int,
  gmtCnt :: Int,
  stdCnt :: Int,
  leapCnt :: Int,
  olsonInfos :: [(Int, Bool, String)],
  isGmt :: [Bool],
  isStd :: [Bool]
  } deriving (Eq,Show)

olsonInfoGet :: Get OlsonInfo
olsonInfoGet = do
  version <- olsonHeader
  case () of
    () | version == '\0' -> olsonInfoGetWith 4 getTime32
    -- ()  -> olsonInfoGetWith 4 getTime32
    () | version `elem` ['2', '3'] -> do
      skipOlson0
      _ <- olsonHeader
      olsonInfoGetWith 8 getTime64
      -- TODO(klao): read the rule string
    _ -> fail $ "olsonGet: invalid version character: " ++ show version

olsonInfoGetWith :: Int -> Get Int64 -> Get OlsonInfo
olsonInfoGetWith szTime getTime = do
  tzh_ttisgmtcnt <- getInt32
  tzh_ttisstdcnt <- getInt32
  tzh_leapcnt <- getInt32
  tzh_timecnt <- getInt32
  tzh_typecnt <- getInt32
  tzh_charcnt <- getInt32
  _transitions <- VU.replicateM tzh_timecnt getTime
  _indices <- VU.replicateM tzh_timecnt getInt8
  infos <- VU.replicateM tzh_typecnt getTTInfo
  abbrs <- getByteString tzh_charcnt
  skip $ tzh_leapcnt * (szTime + 4)
  isStds <- replicateM tzh_ttisstdcnt get
  isGmts <- replicateM tzh_ttisgmtcnt get
  let nameIt (o,d,ni) = (o, d, abbrForInd ni abbrs)
      lInfos = map nameIt $ VU.toList infos
  return $ OlsonInfo tzh_timecnt tzh_typecnt tzh_ttisgmtcnt tzh_ttisstdcnt tzh_leapcnt lInfos isGmts isStds

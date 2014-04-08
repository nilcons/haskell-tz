{- |
Module      : Data.Time.Zones.All
Copyright   : (C) 2014 Mihaly Barasz
License     : Apache-2.0, see LICENSE
Maintainer  : Mihaly Barasz <klao@nilcons.com>
Stability   : experimental
-}

module Data.Time.Zones.All (
  toTZName,
  fromTZName,
  tzNameLabelMap,
  tzByLabel,
  tzByName,
  TZLabel(..),
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Time.Zones.DB
import Data.Time.Zones.Read
import Data.Time.Zones.Types
import qualified Data.Vector as V


-- | Lookup a `TZ` by its label.
--
-- >utcToNewYork :: UTCTime -> LocalTime
-- >utcToNewYork = utcToLocalTimeTZ $ tzByLabel America__New_York
tzByLabel :: TZLabel -> TZ
tzByLabel = (v V.!) . fromEnum
  where
    v = V.fromList $ go tzDescriptions
    go [] = []
    go (Right (_, _, desc) : zs) = parseOlson desc : go zs
    go (Left _ : zs) = go zs

-- | Lookup a `TZ` by the name of it's location.
--
-- Returns `Nothing` if the location is unknown.
tzByName :: BS.ByteString -> Maybe TZ
tzByName n = tzByLabel `fmap` fromTZName n

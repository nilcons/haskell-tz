{- |
Module      : Data.Time.Zones.Types
Copyright   : (C) 2014 Mihaly Barasz
License     : Apache-2.0, see LICENSE
Maintainer  : Mihaly Barasz <klao@nilcons.com>
Stability   : experimental
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Data.Time.Zones.Types (
  TZ(..),
  utcTZ,
  ) where

import           Control.DeepSeq
import           Data.Data
import           Data.Default
import           Data.Int
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU

data TZ = TZ {
  _tzTrans :: !(VU.Vector Int64),
  _tzDiffs :: !(VU.Vector Int),
  -- TODO(klao): maybe we should store it as a vector of indices and a
  -- (short) vector of expanded 'TimeZone's, similarly to how it's
  -- stored?
  _tzInfos :: !(VB.Vector (Bool, String))   -- (summer, name)
  } deriving (Eq, Show, Typeable, Data, Read)

instance NFData TZ where
  rnf (TZ { _tzInfos = infos }) = rnf infos

-- | The `TZ` definition for UTC.
utcTZ :: TZ
utcTZ = TZ (VU.singleton minBound) (VU.singleton 0) (VB.singleton (False, "UTC"))

instance Default TZ where
  def = utcTZ

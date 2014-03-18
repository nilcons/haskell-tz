module Data.Time.Zones.Types (
  TZ(..),
  ) where

import Data.Int
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as VB

data TZ = TZ {
  _tzTrans :: !(VU.Vector Int64),
  _tzDiffs :: !(VU.Vector Int),
  -- TODO(klao): maybe we should store it as a vector of indices and a
  -- (short) vector of expanded 'TimeZone's, similarly to how it's
  -- stored?
  _tzInfos :: !(VB.Vector (Bool, String))   -- (summer, name)
  } deriving (Eq,Show)

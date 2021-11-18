{- |
Module      : Data.Time.Zones.Types
Copyright   : (C) 2014 Mihaly Barasz
License     : Apache-2.0, see LICENSE
Maintainer  : Mihaly Barasz <klao@nilcons.com>
Stability   : experimental
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}

module Data.Time.Zones.Types (
  TZ(..),
  utcTZ,
  PosixTz(..),
  utcPosixTz,
  PosixZone(..),
  TzRule(..),
  TzRuleTy(..),
  ) where

import           Control.DeepSeq
import           Data.Data
import           Data.Default
import Data.Int ( Int64 )
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString as B


data TZ = TZ {
  _tzTrans :: !(VU.Vector Int64),
  _tzDiffs :: !(VU.Vector Int),
  -- TODO(klao): maybe we should store it as a vector of indices and a
  -- (short) vector of expanded 'TimeZone's, similarly to how it's
  -- stored?
  _tzInfos :: !(VB.Vector (Bool, String)),   -- (summer, name)
  _tzPosixTz :: !(Maybe PosixTz)             -- v1 tzfiles do not contain POSIX-TZ rules
  } deriving (Eq, Show, Typeable, Data, Read)

instance NFData TZ where
  rnf (TZ { _tzInfos = infos }) = rnf infos

-- | The `TZ` definition for UTC.
utcTZ :: TZ
utcTZ = TZ (VU.singleton minBound) (VU.singleton 0) (VB.singleton (False, "UTC")) (Just utcPosixTz)

instance Default TZ where
  def = utcTZ

-- | POSIX-TZ variable-type-string data
--
-- POSIX-TZ-environment-variable-style string is used for handling instants after the last
-- transition time stored in the file or for all instants if the file has no transitions
--
-- See:
-- http://www.gnu.org/software/libc/manual/html_node/TZ-Variable.html
--
-- Manual pages: tzfile(5) newtzset(3)
data PosixTz = PosixTz
  { _posixTzStd :: !PosixZone
     -- ^ std name and offset
  , _posixTzDst :: !(Maybe (PosixZone, TzRule, TzRule))
     -- ^ dst name and offset, dst start rule, dst end rule
  } deriving (Eq, Show, Data, Read)

-- | Designation for the standard (std) or summer (dst) time zone together with offset from UTC
data PosixZone = PosixZone
  { _pzName :: !B.ByteString
  , _pzOffset :: !Int
  -- ^ offset in seconds
  --
  -- The offset specifies the time value you must add to the local time to get
  -- a Coordinated Universal Time value.
  } deriving (Eq, Show, Data, Read)

-- | Indicates when to change to or back from summer time
--
-- Field interpretation depends on type.
data TzRule = TzRule
  { _tzrType :: !TzRuleTy
  , _tzrMon  :: {-# UNPACK #-} !Int
  , _tzrNum  :: {-# UNPACK #-} !Int
  , _tzrDay  :: {-# UNPACK #-} !Int
  , _tzrTime :: {-# UNPACK #-} !Int
  } deriving (Eq, Show, Data, Read)

data TzRuleTy
  = TzRuleJ -- Jn
  | TzRuleN -- n
  | TzRuleM -- M.m.w.d
  deriving (Eq, Show, Data, Read)

-- | POSIX-TZ value for UTC (zero offset, no DST)
utcPosixTz :: PosixTz
utcPosixTz = PosixTz (PosixZone "UTC" 0) Nothing

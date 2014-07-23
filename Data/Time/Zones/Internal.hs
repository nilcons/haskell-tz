{- |
Module      : Data.Time.Zones.Internal
Copyright   : (C) 2014 Mihaly Barasz
License     : Apache-2.0, see LICENSE
Maintainer  : Mihaly Barasz <klao@nilcons.com>
Stability   : experimental
-}

{-# LANGUAGE TemplateHaskell #-}

module Data.Time.Zones.Internal (
  picoToInteger,
  integerToPico,
  diffTimeToPico,
  picoToDiffTime,
  diffTimeToInteger,
  integerToDiffTime,
  ) where

import Data.Fixed
import Data.Time
import Data.Time.Zones.Internal.CoerceTH


-- TODO(klao): Is it better to inline them saturated or unsaturated?

picoToInteger :: Pico -> Integer
picoToInteger p = $(destructNewType ''Fixed) p
{-# INLINE picoToInteger #-}

integerToPico :: Integer -> Pico
integerToPico i = $(constructNewType ''Fixed) i
{-# INLINE integerToPico #-}

diffTimeToPico :: DiffTime -> Pico
diffTimeToPico dt = $(destructNewType ''DiffTime) dt
{-# INLINE diffTimeToPico #-}

picoToDiffTime :: Pico -> DiffTime
picoToDiffTime p = $(constructNewType ''DiffTime) p
{-# INLINE picoToDiffTime #-}

diffTimeToInteger :: DiffTime -> Integer
diffTimeToInteger dt = picoToInteger (diffTimeToPico dt)
{-# INLINE diffTimeToInteger #-}

integerToDiffTime :: Integer -> DiffTime
integerToDiffTime i = picoToDiffTime (integerToPico i)
{-# INLINE integerToDiffTime #-}

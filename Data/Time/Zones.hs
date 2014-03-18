{-# LANGUAGE BangPatterns #-}

module Data.Time.Zones (
  module Data.Time.Zones.Types,
  diffForUTC,
  loadTZ,
  ) where

import Data.Bits
import Data.Int
import qualified Data.Vector.Unboxed as VU
import Data.Time.Zones.Types
import Data.Time.Zones.Read (loadTZ)

diffForUTC :: TZ -> Int64 -> Int
{-# INLINE diffForUTC #-}
diffForUTC (TZ ts ds _) t = VU.unsafeIndex ds $ binarySearch ts t

-- | Returns the largest index `i` such that `v ! i <= t`.
--
-- Assumes that `v` is sorted, has at least one element and `v ! 0 <= t`.
binarySearch :: (VU.Unbox a, Ord a) => VU.Vector a -> a -> Int
{-# INLINE binarySearch #-}
binarySearch v t | n == 1    = 0
                 | otherwise = t `seq` go 1 n
  where
    n = VU.length v
    go !l !u | l >= u = (l - 1)
             | VU.unsafeIndex v k <= t  = go (k+1) u
             | otherwise  = go l k
      where
        k = (l + u) `shiftR` 1


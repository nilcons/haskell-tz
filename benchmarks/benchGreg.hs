{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Criterion.Main
import Control.Lens
import Data.Bits
import Data.Time
import qualified Data.Thyme.Calendar.OrdinalDate as Th
import qualified Data.Thyme as Th
import Data.Time.Calendar.OrdinalDate

data YD = YD {
  _ydYear :: {-# UNPACK #-} !Int,
  _ydDay  :: {-# UNPACK #-} !Int
  } deriving (Eq, Show)

makeLenses ''YD

toOrdinalDate' :: Int -> YD
{-# INLINE toOrdinalDate' #-}
toOrdinalDate' !day = YD (fromIntegral y) (fromIntegral d)
  where
    (!y,!d) = toOrdinalDate $ ModifiedJulianDay $ fromIntegral day

toOrdinalDateTh :: Int -> YD
{-# INLINE toOrdinalDateTh #-}
toOrdinalDateTh !day = YD y d
  where
    Th.OrdinalDate y d = Th.ModifiedJulianDay day ^. Th.ordinalDate

toOrdinalDateI :: Int -> YD
toOrdinalDateI !mjd = YD year yd
  where
    a = mjd + 678575
    quadcent = div a 146097
    b = mod a 146097
    cent = min (div b 36524) 3
    c = b - (cent * 36524)
    quad = div c 1461
    d = mod c 1461
    y = min (div d 365) 3
    yd = d - (y * 365) + 1
    year = quadcent * 400 + cent * 100 + quad * 4 + y + 1

toOrdinalDateDivMod :: Int -> YD
{-# NOINLINE toOrdinalDateDivMod #-}
toOrdinalDateDivMod !mjd = YD year yd
  where
    a = mjd + 678575
    (quadcent,b) = a `divMod` 146097
    cent = min (b `quot` 36524) 3
    c = b - (cent * 36524)
    (!quad,!d) = c `quotRem` 1461
    y = min (d `quot` 365) 3
    yd = d - (y * 365) + 1
    year = quadcent * 400 + cent * 100 + quad * 4 + y + 1

--------------------------------------------------------------------------------
-- Brief description of the toOrdinal computation.
--
-- The length of the years in Gregorian calendar is periodic with
-- period of 400 years. There are 100 - 4 + 1 = 97 leap years in a
-- period, so the average length of a year is 365 + 97/400 =
-- 146097/400 days.
--
-- Now, if you consider these -- let's call them nominal -- years,
-- then for any point in time, for any linear day number we can
-- determine which nominal year does it fall into by a single
-- division. Moreover, if we align the start of the calendar year 1
-- with the start of the nominal year 1, then the calendar years and
-- nominal years never get too much out of sync. Specifically:
--
--  * start of the first day of a calendar year might fall into the
--    preceding nominal year, but never more than by 1.5 days (591/400
--    days, to be precise)
--  * the start of the last day of a calendar year always falls into
--    its nominal year (even for the leap years).
--
-- So, to find out the calendar year for a given day, we calculate
-- which nominal year does its start fall. And, if we are not too
-- close to the end of year, we have the right calendar
-- year. Othewise, we just check whether it falls within the next
-- calendar year.
--
-- Notes: to make the reasoning simpler and more efficient ('quot' is
-- faster than 'div') we do the computation directly only for positive
-- years (days after 1-1-1). For earlier dates we "transate" by an
-- integral number of 400 year periods, do the computation and
-- translate back.

isLeapYearI :: Int -> Bool
{-# INLINABLE isLeapYearI #-}
isLeapYearI !y = y .&. 3 == 0  &&  (r100 /= 0 || d100 .&. 3 == 0)
  where
    (d100,r100) = y `quotRem` 100

countLeapYears :: Int -> Int
{-# INLINE countLeapYears #-}
countLeapYears !ys = ys `shiftR` 2  -  cs  +  cs `shiftR` 2
  where
    cs = ys `quot` 100

toOrdinalImproved :: Int -> YD
{-# INLINABLE toOrdinalImproved #-}
toOrdinalImproved !mjd
  | b0 <= 0 = toOrdinalB0 m & ydYear +~ d * 400
  | otherwise = toOrdinalB0 b0
  where
    b0 = mjd + 678575
    (d,m) = b0 `divMod` 146097

toOrdinalB0 :: Int -> YD
{-# INLINE toOrdinalB0 #-}
toOrdinalB0 dayB0 = res
  where
    (y0, r) = (400 * dayB0) `quotRem` 146097
    dayInYear y = dayB0 - 365 * y - countLeapYears y + 1
    d0 = dayInYear y0
    d1 = dayInYear (y0 + 1)
    res = if r >= 146097 - 591 && d1 > 0
          then YD (y0 + 2) d1
          else YD (y0 + 1) d0

checkToOrdinalImproved :: [(Int, (YD, YD))]
checkToOrdinalImproved = filter (uncurry (/=) . snd) l
  where
    l = [ (d, (toOrdinalDate' d', toOrdinalImproved d'))
        | d <- [0 .. 400 * 366], let d' = d - 678575 ]

benchmarks :: [Benchmark]
benchmarks = [
  bgroup "orig" [
     bench "toOrdinalDate" $ nf toOrdinalDate d1,
     bench "toOrdinalDate'" $ whnf toOrdinalDate' d1i
     ],
  bgroup "thyme" [
     bench "toOrdinalDateTh" $ whnf toOrdinalDateTh d1i
     ],
  bgroup "optim" [
     bench "directCopy" $ whnf toOrdinalDateI d1i,
     bench "divModQuotRem" $ whnf toOrdinalDateDivMod d1i,
     bench "Improved" $ whnf toOrdinalImproved d1i
     ]
  ]
  where
    d1 = fromGregorian 2014 3 25
    d1i = fromIntegral $ toModifiedJulianDay d1

main :: IO ()
main = do
  defaultMain benchmarks

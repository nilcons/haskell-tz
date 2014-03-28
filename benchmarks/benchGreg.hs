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
  } deriving (Show)

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

toOrdinalB0 :: Int -> YD
{-# INLINE toOrdinalB0 #-}
toOrdinalB0 b0 = res
  where
    (y, r) = (400 * b0) `quotRem` 146097
    ls = countLeapYears y
    res = if r < 146097 - 400
          then YD y (b0 - 365*y - ls)
          else let y' = if isLeapYearI (y+1) then y else y+1
               in YD y' (b0 - 365*y' - ls)

toOrdinalPr :: Int -> YD
{-# INLINABLE toOrdinalPr #-}
toOrdinalPr !mjd | b0 >= 0 = toOrdinalB0 b0
                 | otherwise = toOrdinalB0 m & ydYear +~ d * 400
  where
    b0 = mjd + 678575
    (d,m) = b0 `divMod` 146097

test :: Integer -> Int -> Int -> IO ()
test y m md = do
  let d = fromIntegral $ toModifiedJulianDay $ fromGregorian y m md
  print $ d + 678575
  print $ toOrdinalDate' d
  let YD y' yd = toOrdinalPr d
  print $ YD (y'+1) (yd+1)


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
     bench "divModPr" $ whnf toOrdinalPr d1i
     ]
  ]
  where
    d1 = fromGregorian 2014 3 25
    d1i = fromIntegral $ toModifiedJulianDay d1

main :: IO ()
main = do
  defaultMain benchmarks

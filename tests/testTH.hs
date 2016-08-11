{-# LANGUAGE TemplateHaskell #-}

import Data.Time.Zones
import Data.Time.Zones.TH
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit hiding (Test, assert)

tzBudapest :: TZ
tzBudapest = $(includeTZFromDB "Europe/Budapest")

case_Budapest_is_Budapest :: IO ()
case_Budapest_is_Budapest = do
  readBp <- loadTZFromDB "Europe/Budapest"
  tzBudapest @?= readBp

main :: IO ()
main = do
  $defaultMainGenerator

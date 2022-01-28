{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Time.Zones
import Data.Time.Zones.All
import Test.Tasty.HUnit
import Test.Tasty.TH

case_Budapest_is_Budapest :: IO ()
case_Budapest_is_Budapest = do
  readBp <- loadTZFromDB "Europe/Budapest"
  readBp @=? tzByLabel Europe__Budapest
  let Just budByName = tzByName "Europe/Budapest"
  readBp @=? budByName

main :: IO ()
main = do
  $defaultMainGenerator

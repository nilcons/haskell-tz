{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Maybe
import Data.Time.Zones
import Data.Time.Zones.All
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit hiding (Test, assert)
import System.Posix.Env

case_Budapest_is_Budapest :: IO ()
case_Budapest_is_Budapest = do
  readBp <- loadTZFromDB "Europe/Budapest"
  readBp @=? tzByLabel Europe__Budapest
  let Just budByName = tzByName "Europe/Budapest"
  readBp @=? budByName

case_aliases :: IO ()
case_aliases = do
  America__Argentina__Buenos_Aires @=? fromJust (fromTZName "America/Buenos_Aires")

case_fromToName :: IO ()
case_fromToName = forM_ [minBound .. maxBound] t
  where
    t :: TZLabel -> IO ()
    t label = Just label @=? fromTZName (toTZName label)

main :: IO ()
main = do
  -- When we are running 'cabal test' the package is not yet
  -- installed, so we want to use the data directory from within the
  -- sources.
  setEnv "tz_datadir" "./tzdata" True
  $defaultMainGenerator

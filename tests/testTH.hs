{-# LANGUAGE TemplateHaskell #-}

import Data.Time.Zones
import Data.Time.Zones.TH

bp :: TZ
bp = $(includeSystemTZ "Europe/Budapest")

main :: IO ()
main = do
  print bp

{-# LANGUAGE TemplateHaskell #-}

import Data.Time
import Data.Time.Zones
import Data.Time.Zones.TH

tzBudapest :: TZ
tzBudapest = $(includeTZFromDB "Europe/Budapest")

tzLosAngeles :: TZ
tzLosAngeles = $(includeTZFromDB "America/Los_Angeles")

main :: IO ()
main = do
  t <- getCurrentTime
  putStrLn $ "Time in Budapest: " ++ show (utcToLocalTimeTZ tzBudapest t)
  putStrLn $ "Time in Los Angeles: " ++ show (utcToLocalTimeTZ tzLosAngeles t)

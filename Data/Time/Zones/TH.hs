-- |
-- Module      : Data.Time.Zones.TH
-- Copyright   : (C) 2014 Mihaly Barasz
-- License     : Apache-2.0, see LICENSE
-- Maintainer  : Janus Troelsen <ysangkok@gmail.com>
-- Stability   : experimental
--
-- /Example usage:/
--
-- >
-- >{-# LANGUAGE TemplateHaskell #-}
-- >
-- >import Data.Time
-- >import Data.Time.Zones
-- >import Data.Time.Zones.TH
-- >
-- >tzBudapest :: TZ
-- >tzBudapest = $(includeTZFromDB "Europe/Budapest")
-- >
-- >tzLosAngeles :: TZ
-- >tzLosAngeles = $(includeTZFromDB "America/Los_Angeles")
-- >
-- >main :: IO ()
-- >main = do
-- >  t <- getCurrentTime
-- >  putStrLn $ "Time in Budapest: " ++ show (utcToLocalTimeTZ tzBudapest t)
-- >  putStrLn $ "Time in Los Angeles: " ++ show (utcToLocalTimeTZ tzLosAngeles t)
--

{-# LANGUAGE TemplateHaskell #-}

module Data.Time.Zones.TH (
  includeTZFromDB,
  includeSystemTZ,
  includeTZFromFile,
  ) where

import Control.DeepSeq
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time.Zones.Files
import Data.Time.Zones.Read
import Language.Haskell.TH

-- | Generate a `TZ` definition from an entry out of the time zone
-- database shipped with this package.
includeTZFromDB :: String -> Q Exp
includeTZFromDB tzName = do
  desc <- runIO $ timeZonePathFromDB tzName >>= BL.readFile
  parseTZ desc

-- | Generate a `TZ` definition from a system time zone information file.
--
-- See also: `loadSystemTZ` for details on how system time zone files
-- are located.
--
-- Note, this is unlikely to work on non-posix systems (e.g.,
-- Windows), use `includeTZFromDB` or `includeTZFromFile` instead.
includeSystemTZ :: String -> Q Exp
includeSystemTZ tzName = do
  desc <- runIO $ pathForSystemTZ tzName >>= BL.readFile
  parseTZ desc

-- | Generate a `TZ` definition from the given time zone information file.
includeTZFromFile :: FilePath -> Q Exp
includeTZFromFile fname = do
  desc <- runIO $ BL.readFile fname
  parseTZ desc

--------------------------------------------------------------------------------
-- Template Haskell helper functions.

-- Why the round-trip through `String`? Why don't we generate a fully
-- expanded definition of `TZ`?
--
-- First, we want a definition that is stored compactly in the
-- resulting binary, and 'String' literals are stored as C strings.
--
-- Secondly, vectors (which are the internal representation of TZ)
-- don't have literal representation, so we couldn't produce a
-- fully-evaluated representation anyway. Also, it would be much more
-- complicated.
--
parseTZ :: BL.ByteString -> Q Exp
parseTZ desc = do
  -- Check that the description actually parses, so if there's a bug
  -- we fail at compile time and not at run time:
  parseOlson desc `deepseq` return ()
  [| parseOlson (BL.pack $(stringE $ BL.unpack desc)) |]

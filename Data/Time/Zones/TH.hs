-- |
-- Module      : Data.Time.Zones.TH
-- Copyright   : (C) 2014 Mihaly Barasz
-- License     : Apache-2.0, see LICENSE
-- Maintainer  : Mihaly Barasz <klao@nilcons.com>
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

{-# OPTIONS_HADDOCK prune #-}

module Data.Time.Zones.TH (
  includeTZFromDB,
  includeSystemTZ,
  includeTZFromFile,
  -- Internal functions
  parseTZInternal,
  ) where

import Control.DeepSeq
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time.Zones.Read
import Data.Time.Zones.Types
import Data.Version
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Paths_tz

-- | Generate a `TZ` definition from an entry out of the time zone
-- database shipped with this package.
includeTZFromDB :: String -> Q Exp
includeTZFromDB tzName = do
  desc <- runIO $ tzDescriptionFromDB tzName
  parseTZ desc

-- | Generate a `TZ` definition from a system time zone information file.
--
-- See also: `loadSystemTZ` for details on how system time zone files
-- are located.
includeSystemTZ :: String -> Q Exp
includeSystemTZ tzName = do
  desc <- runIO $ systemTZDescription tzName
  parseTZ desc

-- | Generate a `TZ` definition from the given time zone information file.
includeTZFromFile :: FilePath -> Q Exp
includeTZFromFile fname = do
  desc <- runIO $ tzDescriptionFromFile fname
  parseTZ desc

--------------------------------------------------------------------------------
-- Template Haskell helper functions.

-- A bit of a rationale about this implementation.
--
-- 1. The implementation below is basically a convoluted version of
-- the following:
--
--     parseTZ :: BL.ByteString -> Q Exp
--     parseTZ desc =
--       [| parseTZDescription (BL.pack $(stringE $ BL.unpack desc)) |]
--
-- So, why the complications?
--
-- Why we want to _provide_ the possibility for the users to define
-- TZs with Template Haskell, we do not want to _depend_ on Template
-- Haskell in _this package_ itself. This way @tz@ can potentially be
-- cross-compiled.
--
-- 2. Why the round-trip through `String`? Why don't we generate a
-- fully expanded definition of `TZ`?
--
-- First, we want a definition that is stored compactly in the
-- resulting binary, and `String` literals are stored as C strings.
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
  parseTZDescription desc `deepseq` return ()
  parseTZInternalName <- getLocalName "parseTZInternal"
  appE (varE parseTZInternalName) $ stringE $ BL.unpack desc

-- Create a `Name` of the safe form that value name quoting
-- (ie. 'function) creates.
globalName :: String -> String -> String -> Name
globalName name modName package =
   Name (OccName name) (NameG VarName (PkgName package) (ModName modName))

-- This is an imperfect substitute of name quoting
-- (eg. 'parseTZInternalName), which again we are doing because we
-- don't want to use the TemplateHaskell extension.
--
-- If you have the @tz@ package installed and just using it; or if you
-- are building this package with Cabal, the parseTZInternal name is
-- found in the package "tz-<version>".
-- But, if you are just debugging things in this package and compiling
-- stuff with ghc by hand, it will be found in the "main" package. So,
-- we first construct a global name as if it were in the "main"
-- package. Then, we try to reify it, which will fail in the normal
-- (first) case, in which case we fall back to "tz-<version>".
getLocalName :: String -> Q Name
getLocalName functionName = do
  let nameInPackage = globalName functionName "Data.Time.Zones.TH"
  recover (return $ nameInPackage $ "tz-" ++ showVersion version) $ do
    let name = nameInPackage "main"
    _ <- reify name
    return name

-- Internal function used by spliced `TZ` definitions
--
-- This function has to be exported, so that it can be found at the
-- place of splicing.
parseTZInternal :: String -> TZ
{-# INLINE parseTZInternal #-}
parseTZInternal = parseTZDescription . BL.pack

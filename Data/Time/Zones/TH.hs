{- |
Module      : Data.Time.Zones.TH
Copyright   : (C) 2014 Mihaly Barasz
License     : Apache-2.0, see LICENSE
Maintainer  : Mihaly Barasz <klao@nilcons.com>
Stability   : experimental
-}

{-# LANGUAGE TemplateHaskell #-}

module Data.Time.Zones.TH (
  includeTZFromDB,
  includeSystemTZ,
  includeTZFromFile,
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time.Zones.Read
import Language.Haskell.TH

parseTZ :: BL.ByteString -> Q Exp
parseTZ desc =
  [| parseTZDescription (BL.pack $(stringE $ BL.unpack desc)) |]

includeTZFromDB :: String -> Q Exp
includeTZFromDB tzName = do
  desc <- runIO $ tzDescriptionFromDB tzName
  parseTZ desc

includeSystemTZ :: String -> Q Exp
includeSystemTZ tzName = do
  desc <- runIO $ systemTZDescription tzName
  parseTZ desc

includeTZFromFile :: FilePath -> Q Exp
includeTZFromFile fname = do
  desc <- runIO $ tzDescriptionFromFile fname
  parseTZ desc

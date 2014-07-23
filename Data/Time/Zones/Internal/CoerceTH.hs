{- |
Module      : Data.Time.Zones.Internal.CoerceTH
Copyright   : (C) 2014 Mihaly Barasz
License     : Apache-2.0, see LICENSE
Maintainer  : Mihaly Barasz <klao@nilcons.com>
Stability   : experimental
-}

{-# LANGUAGE TemplateHaskell #-}

module Data.Time.Zones.Internal.CoerceTH (
  getNewTypeCon,
  constructNewType,
  destructNewType,
  ) where

import Language.Haskell.TH

getNewTypeCon :: Name -> Q Name
getNewTypeCon newTy = do
  info <- reify newTy
  case info of
    TyConI (NewtypeD _ _ _ (NormalC name _) _) -> return name
    _ -> fail "Not a newtype"

constructNewType :: Name -> Q Exp
constructNewType newTy = ConE `fmap` getNewTypeCon newTy

destructNewType :: Name -> Q Exp
destructNewType newTy = do
  con <- getNewTypeCon newTy
  lamV <- newName "x"
  patV <- newName "v"
  return $
    LamE [VarP lamV]
    (CaseE (VarE lamV) [
        Match (ConP con [VarP patV]) (NormalB (VarE patV)) []])

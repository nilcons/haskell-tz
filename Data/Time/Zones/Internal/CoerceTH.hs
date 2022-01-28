{- |
Module      : Data.Time.Zones.Internal.CoerceTH
Copyright   : (C) 2014 Mihaly Barasz
License     : Apache-2.0, see LICENSE
Maintainer  : Mihaly Barasz <klao@nilcons.com>
Stability   : experimental
-}

{-# LANGUAGE CPP #-}
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
#if MIN_VERSION_template_haskell(2,11,0)
    TyConI (NewtypeD _ _ _ _ (NormalC name _) _) -> return name
#else
    TyConI (NewtypeD _ _ _ (NormalC name _) _) -> return name
#endif
    _ -> fail "Not a newtype"

constructNewType :: Name -> Q Exp
constructNewType newTy = ConE `fmap` getNewTypeCon newTy

mkConP :: Name -> [Pat] -> Pat
mkConP name pats =
#if MIN_VERSION_template_haskell(2,18,0)
  ConP name [] pats
#else
  ConP name pats
#endif

destructNewType :: Name -> Q Exp
destructNewType newTy = do
  con <- getNewTypeCon newTy
  lamV <- newName "x"
  patV <- newName "v"
  return $
    LamE [VarP lamV]
    (CaseE (VarE lamV) [
        Match (mkConP con [VarP patV]) (NormalB (VarE patV)) []])

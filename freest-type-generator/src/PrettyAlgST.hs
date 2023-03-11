{-# LANGUAGE LambdaCase #-}

module PrettyAlgSt where

import qualified Control.Monad.Reader as R

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass as P
import Types

import PrettyShared

data PPEnv =
  PPENV

prettyType :: Type -> R.Reader PPEnv Doc
prettyType = \case
  TyVar n -> pure $ text n
  TyUnit  -> pure $ text "()"
  TyBase n -> pure $ text n
  TyArrow a r -> do
    pa <- prettyType a
    pr <- prettyType r
    pure $ parens (pa <+> text "->" <+> pr)
  TyLolli a r -> do
    pa <- prettyType a
    pr <- prettyType r
    pure $ parens (pa <+> text "-o" <+> pr)
  TyPair f s -> do
    pf <- prettyType f
    ps <- prettyType s
    pure $ parens (pf <+> comma <+> ps)
  TyPoly n k b -> do
    pb <- prettyType b
    pure $ parens (text "forall" <+> parens (text n <+> colon <+> pPrint k) <+> dot <+> pb)
  TyApp n [] -> do
    pure $ text n
  TyApp n args -> do
    pargs <- mapM prettyType args
    pure $ parens (foldl (<+>) (text n) pargs)
  TySession d t s -> do
    pt <- prettyType t
    ps <- prettyType s
    pure $ parens (pPrint d <+> pt <+> dot <+> ps)
  TyEnd d -> pure $ (text "End" P.<> pPrint d)
  TyDual b -> do
    pb <- prettyType b
    pure $ parens (text "Dual" <+> pb)

prettyProtocol :: Protocol -> R.Reader PPEnv Doc
prettyProtocol p = do
  pctors <- mapM prettyConstructor (prCtors p)
  pure $ hang (text "protocol" <+>
               foldl (<+>) (text (prName p)) (map text (prParameters p)) <+>
               equals
              )
              4
              (vcat $ punctuate bar pctors)

prettyConstructor :: Constructor -> R.Reader PPEnv Doc
prettyConstructor c = do
  pargs <- mapM prettyArgument (ctArgs c)
  pure $ foldl (<+>) (text (ctName c)) pargs

prettyArgument :: Argument -> R.Reader PPEnv Doc
prettyArgument arg = do
  pt <- prettyType (argType arg)
  pure $ (pPrint (argPolarity arg) P.<> pt)

prettyModule :: Module -> R.Reader PPEnv Doc
prettyModule (Module ps ts) = do
  pps <- mapM prettyProtocol ps
  pts <- mapM prettyType ts
  pure (vcat pps $$ vcat pts)

pretty :: R.Reader PPEnv Doc -> String
pretty reader = 
  let init = PPENV in
  render (R.runReader reader init)

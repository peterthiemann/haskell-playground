{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module PrettyAlgST (prettyType, prettyModule, runPretty) where

import Control.Monad
import Control.Monad.Reader qualified as R

import PrettyShared
import Types

data PPEnv =
  PPENV

prettySession :: TySession -> R.Reader PPEnv Doc
prettySession = \case
  SeVar n -> pure $ pPrint n
  TyTransmit d t s -> do
    pt <- prettyTyProto t
    ps <- prettySession s
    pure $ parens (pPrint d <> pt <+> dot <+> ps)
  TyEnd d -> pure (text "End" <> pPrint d)
  TyDual b -> do
    pb <- prettySession b
    pure $ parens (text "dual" <+> pb)

prettyType :: Type -> R.Reader PPEnv Doc
prettyType = \case
  TyVar n -> pure $ pPrint n
  TyUnit  -> pure $ text "()"
  TyBase n -> pure $ pPrint n
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
    pure $ parens (text "forall" <+> parens (pPrint n <> colon <> pPrint k) <> dot <+> pb)
  TySession s ->
    prettySession s

prettyTyProto :: TyProto -> R.Reader PPEnv Doc
prettyTyProto = \case
  TyApp n [] -> do
    pure $ pPrint n
  TyApp n args -> do
    pargs <- mapM prettyTyProto args
    pure $ parens (foldl (<+>) (pPrint n) pargs)
  TyPVar pv ->
    pure $ pPrint pv
  TyNeg t -> do
    t' <- prettyTyProto t
    pure $ parens $ "-" <> t'
  TyType t ->
    prettyType t

prettyProtocol :: Protocol -> R.Reader PPEnv Doc
prettyProtocol p = do
  pctors <- mapM prettyConstructor (prCtors p)
  let prparam param =
        parens (pPrint param <> colon <> "P")
  let prhead =
        "protocol"
          <+> foldl (<+>) (pPrint (prName p)) (prparam <$> prParameters p)
          <+> equals
  let prdef = vcat $
        prhead : ["  " <> bar <+> ctor | ctor <- pctors]
  pure $ prdef <> nl

prettyConstructor :: Constructor -> R.Reader PPEnv Doc
prettyConstructor c = do
  pargs <- mapM prettyArgument (ctArgs c)
  pure $ foldl (<+>) (pPrint (ctName c)) pargs

prettyArgument :: Argument -> R.Reader PPEnv Doc
prettyArgument arg = do
  pt <- prettyTyProto (argType arg)
  pure (pPrint (argPolarity arg) <> pt)

prettyBenchmark :: Bool -> Int -> Two Type -> R.Reader PPEnv Doc
prettyBenchmark eqChecks i (Two t u) = do
  pt <- prettyType t
  pu <- prettyType u
  let tn c = "T" <> intDoc i <> c
      eqfn c = "eq" <> intDoc i <> c
  let typeDoc = vcat
        [ "type" <+> tn "a" <+> equals <+> pt
        , "type" <+> tn "b" <+> equals <+> pu
        , ""
        ]
  let benchDoc = vcat
        [ "{-# BENCHMARK"
        , tn "a"
        , tn "b"
        , "#-}"
        , ""
        ]
  let eqDoc = vcat
        [ eqfn "a" <+> colon <+> tn "a" <+> "->" <+> tn "b"
        , eqfn "a" <+> "x" <+> equals <+> "x"
        , eqfn "b" <+> colon <+> tn "b" <+> "->" <+> tn "a"
        , eqfn "b" <+> "x" <+> equals <+> "x"
        , ""
        ]
  pure $ if eqChecks
    then typeDoc $$ eqDoc $$ benchDoc
    else typeDoc $$ benchDoc

prettyModule :: Bool -> Module -> R.Reader PPEnv Doc
prettyModule eqChecks (Module ps ts) = do
  pps <- mapM prettyProtocol ps
  pts <- zipWithM (prettyBenchmark eqChecks) [1..] ts
  pure (vcat pps $$ vcat pts)

runPretty :: R.Reader PPEnv Doc -> Doc
runPretty = flip R.runReader PPENV

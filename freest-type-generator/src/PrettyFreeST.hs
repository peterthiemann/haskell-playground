{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module PrettyFreeST (prettyType, prettyModule, runPretty) where

import qualified Control.Monad.Reader as R
import qualified Data.Map as M
import Data.Char (toLower)
import Types

import PrettyShared 

-- input is a list of protocol definitions and a type
-- assume that the type is well-kinded etc
-- it should not contain polymorphic recursion as this is not expressible in FreeST

-- should be OK for monomorphic protocols
-- shaky for parameterized ones:
--  it should instantiate the parameters before looking up, but right now it only has the text of the instantiation

type PrettyM = R.Reader PPEnv

pName :: Name -> Doc
pName = text . map toLower . fromName

pParam :: Param -> Doc
pParam = text . map toLower . fromParam

pKind :: Kind -> Doc
pKind SL = text "1S"
pKind TL = text "1T"
pKind TU = text "*T"

data PPEnv =
  PPENV { ppDefs :: [(Param, Doc)]
        , ppProto :: [Protocol]
        , ppCache :: M.Map (Direction, Name, [TyProto]) Name
        , ppGen   :: Integer
        }

pushDefs :: [Param] -> [Doc] -> PPEnv -> PPEnv
pushDefs ns ds ppenv = ppenv { ppDefs = zip ns ds ++ ppDefs ppenv }

lookupCache :: Direction -> Name -> [TyProto] -> PrettyM (Maybe Name)
lookupCache dir name args = R.asks $ M.lookup (dir, name, args) . ppCache

pushCache :: Direction -> Name -> [TyProto] -> Name -> PPEnv -> PPEnv
pushCache dir name args recVar ppenv =
  ppenv{ ppCache = M.insert (dir, name, args) recVar (ppCache ppenv) }

incGen :: PPEnv -> PPEnv
incGen ppenv = ppenv { ppGen = 1 + ppGen ppenv }

askGen ::  PrettyM Integer
askGen = fmap ppGen R.ask

askDefs :: PrettyM [(Param, Doc)]
askDefs = fmap ppDefs R.ask

lookupProtocol :: Name -> PrettyM Protocol
lookupProtocol pn = R.asks \env ->
  case filter ((pn ==) . prName) (ppProto env) of
    [] -> error $ "Bad protocol name `" ++ fromName pn ++ "`"
    p : _ -> p

prettyChoice :: Direction -> Doc
prettyChoice Input = text "&"
prettyChoice Output = text "+"

prettySession :: TySession -> PrettyM Doc
prettySession = \case
  SeVar n -> do
    defs <- askDefs
    case lookup n defs of
      Nothing ->
        pure $ pParam n
      Just doc ->
        pure doc
  TyTransmit d t s -> do
    pdt <- prettyAsProtocol d t
    ps <- prettySession s
    pure $ parens (pdt <+> semi <+> ps)
  TyEnd _d -> pure $ text "Skip"
  TyDual b -> do
    pb <- prettySession b
    pure $ parens (text "dualof" <+> pb)
    

prettyType :: Type -> PrettyM Doc
prettyType = \case
  TyVar n _ -> do
    defs <- askDefs
    case lookup n defs of
      Nothing ->
        pure $ pParam n
      Just doc ->
        pure doc
  TyUnit -> pure $ text "()"
  TyBase n -> pure $ pPrint n
  TyArrow a r -> do
    pa <- prettyType a
    pr <- prettyType r
    pure $ parens (pa <+> text "*->" <+> pr)
  TyLolli a r -> do
    pa <- prettyType a
    pr <- prettyType r
    pure $ parens (pa <+> text "1->" <+> pr)
  TyPair f s -> do
    pf <- prettyType f
    ps <- prettyType s
    pure $ parens (pf <+> comma <+> ps)
  TyPoly n k b -> do
    pb <- prettyType b
    pure $ parens (text "forall" <+> pParam n <+> colon <+> pKind k <> dot <+> pb)
  TySession s ->
    prettySession s

prettyAsProtocol :: Direction -> TyProto -> PrettyM Doc
prettyAsProtocol d = \case
  TyApp n args ->
    prettyProtocol d n args
  TyPVar pv ->
    pure $ pParam pv
  TyNeg t ->
    prettyAsProtocol (dualDirection d) t
  TyType t -> do
    pt <- prettyType t
    pure $ parens (pPrint d <> pt)

prettyProtocol :: Direction -> Name -> [ TyProto ] -> PrettyM Doc
prettyProtocol d pn ts = do
  mn <- lookupCache d pn ts
  case mn of
    Just recVar -> pure $ pName recVar
    Nothing -> do
      -- We need to know the protocol's constructors.
      protocol <- lookupProtocol pn
      -- Instantiate the constructors.
      let ctors = subst (zip (prParameters protocol) ts) (prCtors protocol)
      pts <- mapM (prettyAsProtocol d) ts
      g <- askGen
      let png = Name (fromName pn ++ show g)
          localize mkctor = R.local incGen $
                            R.local (pushCache d pn ts png) $
                            R.local (pushDefs (prParameters protocol) pts) mkctor
      let recVar = "rec" <+> pName png <+> colon <+> pKind SL <> dot
      parens <$> case ctors of
        [oneCtor] -> do
          pctor <- localize (prettyOneConstructor d oneCtor)
          pure $ recVar <+> pctor
        manyCtors -> do
          pcts <- localize (mapM (prettyConstructor d) manyCtors)
          pure $ recVar <+> prettyChoice d <> braces (sep $ punctuate comma pcts)

prettyConstructor :: Direction -> Constructor -> PrettyM Doc
prettyConstructor d ctor = do
  steps <- prettyOneConstructor d ctor
  pure $ pPrint (ctName ctor) <> colon <+> steps

prettyOneConstructor :: Direction -> Constructor -> PrettyM Doc
prettyOneConstructor d ctor = do
  pargs <- mapM (prettyArgument d) (ctArgs ctor)
  let combine parg prest = parg <+> semi <+> prest
  pure (foldl combine (text "Skip") pargs)

prettyArgument :: Direction -> Argument -> PrettyM Doc
prettyArgument d arg = do
  let d' = case argPolarity arg of
        Plus -> d
        Minus -> dualDirection d
  prettyAsProtocol d' (argType arg)

prettyBenchmark :: Two Type -> PrettyM Doc
prettyBenchmark (Two t u) = do
  pt <- prettyType t
  pu <- prettyType u
  pure $ pt $$ pu

prettyModule :: Module -> PrettyM Doc
prettyModule (Module ps ts) = do
  pts <- R.local (\ ppenv -> ppenv { ppProto = ps }) do
    mapM prettyBenchmark ts
  pure $ vcat (punctuate nl pts) <> nl

runPretty :: PrettyM Doc -> Doc
runPretty = flip R.runReader (PPENV [] [] M.empty 0)

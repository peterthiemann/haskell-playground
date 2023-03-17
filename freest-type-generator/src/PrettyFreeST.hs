{-# LANGUAGE LambdaCase, FlexibleContexts #-}
module PrettyFreeST (prettyType, prettyModule, pretty) where

import qualified Control.Monad.Reader as R
import qualified Data.Map as M
import Data.Char (toLower)
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import Types

import PrettyShared 

-- input is a list of protocol definitions and a type
-- assume that the type is well-kinded etc
-- it should not contain polymorphic recursion as this is not expressible in FreeST

-- should be OK for monomorphic protocols
-- shaky for parameterized ones:
--  it should instantiate the parameters before looking up, but right now it only has the text of the instantiation

pName :: Name -> Doc
pName = text . map toLower . fromName

pParam :: Param -> Doc
pParam = text . map toLower . fromParam

pKind :: Kind -> Doc
pKind SL = text "1S"
pKind SU = text "*S"
pKind TL = text "1T"
pKind TU = text "*T"
pKind ML = text "1T"
pKind MU = text "*T"

data PPEnv =
  PPENV { ppDefs :: [(Param, Doc)]
        , ppProto :: [Protocol]
        , ppCache :: M.Map TyProto Name
        , ppGen   :: Integer
        }

pushDef :: Param -> Doc -> PPEnv -> PPEnv
pushDef n d ppenv = ppenv { ppDefs = (n, d) : ppDefs ppenv }

pushDefs :: [Param] -> [Doc] -> PPEnv -> PPEnv
pushDefs ns ds ppenv = ppenv { ppDefs = zip ns ds ++ ppDefs ppenv }

lookupCache :: TyProto -> R.Reader PPEnv (Maybe Name)
lookupCache t = pure (M.lookup t) <*> fmap ppCache R.ask

pushCache :: TyProto -> Name -> PPEnv -> PPEnv
pushCache t n ppenv = ppenv { ppCache = M.insert t n (ppCache ppenv)}

incGen :: PPEnv -> PPEnv
incGen ppenv = ppenv { ppGen = 1 + ppGen ppenv }

askGen ::  R.Reader PPEnv Integer
askGen = fmap ppGen R.ask

askDefs :: R.Reader PPEnv [(Param, Doc)]
askDefs = fmap ppDefs R.ask

askProtocols :: R.Reader (PPEnv) [Protocol]
askProtocols = fmap ppProto R.ask

prettyChoice :: Direction -> Doc
prettyChoice Input = text "&"
prettyChoice Output = text "+"

prettySession :: TySession -> R.Reader (PPEnv) Doc
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
    

prettyType :: Type -> R.Reader (PPEnv) Doc
prettyType = \case
  TyVar n -> do
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
    pure $ parens (text "forall" <+> pParam n <+> colon <+> pKind k <+> dot <+> pb)
  TySession s ->
    prettySession s

prettyAsProtocol :: Direction -> TyProto -> R.Reader (PPEnv) Doc
prettyAsProtocol d = \case
  TyApp n args ->
    prettyProtocol d n args
  TyPVar pv ->
    pure $ pParam pv
  TyType t -> do
    pt <- prettyType t
    pure $ parens (pPrint d <+> pt)

prettyProtocol :: Direction -> Name -> [ TyProto ] -> R.Reader (PPEnv) Doc
prettyProtocol d pn ts = do
  mn <- lookupCache (TyApp pn ts)
  case mn of
    Just png ->
      pure $ pName png
    Nothing -> do
      ps <- askProtocols
      case lookup pn (map namedProtocol ps) of
        Nothing ->
          pure $ text "Skip" <+> braces (text ("- BAD PROTOCOL NAME " ++ fromName pn ++ " -"))
        Just protocol -> do
          -- instantiate the constructors
          let ctors = subst (zip (prParameters protocol) ts) (prCtors protocol)
          pts <- mapM (prettyAsProtocol d) ts
          g <- askGen
          let png = Name (fromName pn ++ show g)
              localize mkctor = R.local incGen $
                                R.local (pushCache (TyApp pn ts) png) $
                                R.local (pushDefs (prParameters protocol) pts) mkctor
          case ctors of
            [oneCtor] -> do
              pctor <- localize (prettyOneConstructor d oneCtor)
              pure (text "rec" <+> pName png <+> colon <+> pKind SL <+> dot <+> pctor)
            manyCtors -> do
              pcts <- localize (mapM (prettyConstructor d) manyCtors)
              pure (text "rec" <+> pName png <+> colon <+> pKind SL <+> dot <+>
                    prettyChoice d <+> braces (sep $ punctuate comma pcts))

prettyConstructor :: Direction -> Constructor -> R.Reader PPEnv Doc
prettyConstructor d ctor = do
  pargs <- mapM (prettyArgument d) (ctArgs ctor)
  let combine parg prest = parg <+> semi <+> prest
  pure (pPrint (ctName ctor) <+> colon <+> foldr combine (text "Skip") pargs)

prettyOneConstructor :: Direction -> Constructor -> R.Reader PPEnv Doc
prettyOneConstructor d ctor = do
  pargs <- mapM (prettyArgument d) (ctArgs ctor)
  let combine parg prest = parg <+> semi <+> prest
  pure (foldl combine (text "Skip") pargs)

prettyArgument :: Direction -> Argument -> R.Reader (PPEnv) Doc
prettyArgument d arg = do
  let d' = case argPolarity arg of
        Plus -> d
        Minus -> dualDirection d
  prettyAsProtocol d' (argType arg)

prettyModule :: Module -> R.Reader PPEnv Doc
prettyModule (Module ps ts) = do
  pts <- R.local (\ ppenv -> ppenv { ppProto = ps }) $ mapM prettyType ts
  pure (vcat pts)


pretty :: R.Reader PPEnv Doc -> String
pretty reader =
  let ppenv = PPENV [] [] M.empty 0 in
  render (R.runReader reader ppenv)

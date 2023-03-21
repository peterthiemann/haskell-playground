{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
module Types where

import Data.String
import Test.QuickCheck

import PrettyShared

-- | Like @(a, a)@ but with a 'ZipList' like 'Applicative' instance.
data Two a = Two a a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative Two where
  pure a = Two a a
  Two fa fb <*> Two a b = Two (fa a) (fb b)

-- type definitions for protocol definitions and types

newtype Name = Name { fromName :: String }
  deriving (Eq, Ord, Show)
instance IsString Name where
  fromString = Name
newtype Param = Param { fromParam :: String }
  deriving (Eq, Ord, Show)
instance IsString Param where
  fromString = Param

data Polarity = Plus | Minus
  deriving (Eq, Ord, Show, Bounded, Enum)
data Direction = Input | Output
  deriving (Eq, Ord, Show, Bounded, Enum)
data Kind = SL | TL | TU
  deriving (Eq, Ord, Show, Bounded, Enum)

subkind :: Kind -> Kind -> Bool
subkind SL SL = True
subkind SL TL = True
subkind TL TL = True
subkind TU TU = True
subkind TU TL = True
subkind _  _  = False

data Protocol =
  Protocol { prName :: Name
           , prParameters :: [Param]
           , prCtors :: [Constructor]
           }
  deriving (Eq, Show)

data Constructor =
  Constructor { ctName :: Name
              , ctArgs :: [Argument]
              }
  deriving (Eq, Show)

data Argument =
  Argument { argPolarity :: Polarity
           , argType :: TyProto
           }
  deriving (Eq, Show)

data TyProto =
    TyType { tyType :: Type }
  | TyPVar { tpVar :: Param }
  | TyNeg { tpNeg :: TyProto }
  | TyApp { tpName :: Name
          , tyArgs :: [TyProto]
          }
  deriving (Eq, Ord, Show)

data TySession =
    SeVar { seParam :: Param }
  | TyTransmit { tyDirection :: Direction
              , tyPayload :: TyProto
              , tyCont :: TySession
              }
  | TyEnd  { tyDirection :: Direction }
  | TyDual { seBody :: TySession }
  deriving (Eq, Ord, Show)

data Type =
    TyVar { tyParam :: Param
          , tyKind :: Kind
          }
  | TyUnit
  | TyBase { tyName :: Name }
  | TyLolli { tyArg :: Type
            , tyRes :: Type
            }
  | TyArrow { tyArg :: Type
            , tyRes :: Type
            }
  | TyPair { tyFst :: Type
           , tySnd :: Type
           }
  | TyPoly { tyParam :: Param
           , tyKind :: Kind
           , tyBody :: Type
           }
  | TySession { tySession :: TySession }
  deriving (Eq, Ord, Show)

typeKind :: Type -> Kind
typeKind = \case
  TyVar _ k -> k
  TyUnit -> TU
  TyBase _ -> TU
  TyLolli{} -> TL
  TyArrow{} -> TU
  TyPair t u
    | typeKind t == TU && typeKind u == TU -> TU
    | otherwise -> TL
  TyPoly{} -> TU
  TySession _ -> SL

data Module = Module [Protocol] [Two Type]
  deriving (Eq, Show)

namedProtocol :: Protocol -> (Name, Protocol)
namedProtocol p = (prName p, p)

dualDirection :: Direction -> Direction
dualDirection Input = Output
dualDirection Output = Input

----------------------------------------------------------------------
-- sizeOf
----------------------------------------------------------------------

class SizeOf t where
  sizeOf :: t -> Int

instance SizeOf Type where
  sizeOf = \case
    TyVar _ _ -> 1
    TyUnit -> 1
    TyBase _n -> 1
    TyLolli t1 t2 -> 1 + sizeOf t1 + sizeOf t2
    TyArrow t1 t2 -> 1 + sizeOf t1 + sizeOf t2
    TyPair  t1 t2 -> 1 + sizeOf t1 + sizeOf t2
    TyPoly _parm _k ty -> 1 + sizeOf ty
    TySession ts -> sizeOf ts

instance SizeOf TySession where
  sizeOf = \case
    SeVar _parm -> 1
    TyTransmit _d tp tc ->
      1 + sizeOf tp + sizeOf tc
    TyEnd _d ->
      1
    TyDual sy ->
      1 + sizeOf sy

instance SizeOf TyProto where
  sizeOf = \case
    TyType ty -> sizeOf ty
    TyPVar _pv -> 1
    TyNeg ty -> 1 + sizeOf ty
    TyApp _pn pargs -> 1 + sizeOf pargs

instance SizeOf t => SizeOf [t] where
  sizeOf xs = foldr ((+) . sizeOf) 0 xs

instance SizeOf Constructor where
  sizeOf = \case
    Constructor _n args -> 1 + sizeOf args

instance SizeOf Argument where
  sizeOf = \case
    Argument _p tp -> 1 + sizeOf tp

instance SizeOf Protocol where
  sizeOf p = 1 + sizeOf (prParameters p) + sizeOf (prCtors p)

instance SizeOf Param where
  sizeOf _ = 1

----------------------------------------------------------------------
-- substitution
----------------------------------------------------------------------

type Substitution = [(Param, TyProto)]

class Subst t where
  subst :: Substitution -> t -> t

instance Subst Type where
  subst s = \case
    TyVar parm k -> TyVar parm k
    TyUnit -> TyUnit
    TyBase n -> TyBase n
    TyLolli t1 t2 -> TyLolli (subst s t1) (subst s t2)
    TyArrow t1 t2 -> TyArrow (subst s t1) (subst s t2)
    TyPair  t1 t2 -> TyPair (subst s t1) (subst s t2)
    TyPoly parm k ty -> TyPoly parm k (subst s ty)
    TySession ts -> TySession (subst s ts)

instance Subst TySession where
  subst s = \case
    SeVar parm -> SeVar parm
    TyTransmit d tp tc ->
      TyTransmit d (subst s tp) (subst s tc)
    TyEnd d ->
      TyEnd d
    TyDual sy ->
      TyDual (subst s sy)

instance Subst TyProto where
  subst s = \case
    TyType ty -> TyType (subst s ty)
    TyPVar pv ->
      case lookup pv s of
        Nothing -> TyPVar pv
        Just proto -> proto
    TyNeg ty -> TyNeg (subst s ty)
    TyApp pn pargs -> TyApp pn (subst s pargs)

instance Subst Constructor where
  subst s = \case
    Constructor n args -> Constructor n (subst s args)

instance Subst Argument where
  subst s = \case
    Argument p tp -> Argument p (subst s tp)

instance Subst x => Subst [x] where
  subst s = map (subst s)

{-
class Free t where
  free :: t -> S.Set Param

instance Free Type where
  free = \case
    TyVar parm _ -> S.singleton parm
    TyUnit -> S.empty
    TyBase _ -> S.empty
    TyLolli t1 t2 -> S.union (free t1) (free t2)
    TyArrow t1 t2 -> S.union (free t1) (free t2)
    TyPair  t1 t2 -> S.union (free t1) (free t2)
    TyPoly parm _ ty -> free ty S.\\ S.singleton parm
    TySession ts -> free ts

instance Free TySession where
  free = \case
    SeVar parm -> S.singleton parm
    TyTransmit _ tp tc -> S.union (free tp) (free tc)
    TyEnd _ -> S.empty
    TyDual sy -> free sy

instance Free TyProto where
  free = \case
    TyType ty -> free ty
    TyPVar pv -> S.singleton pv
    TyApp _ pargs -> foldl' S.union S.empty (map free pargs)
    TyNeg ty -> free ty
-}

----------------------------------------------------------------------
-- instances for PP
----------------------------------------------------------------------

instance Pretty Kind where
  pPrint = text . show

instance Pretty Direction where
  pPrint Input = text "?"
  pPrint Output = text "!"

instance Pretty Polarity where
  pPrint Plus = mempty
  pPrint Minus = text "-"

instance Pretty Name where
  pPrint (Name s) = text s

instance Pretty Param where
  pPrint (Param s) = text s

----------------------------------------------------------------------
-- instances for QuickCheck
----------------------------------------------------------------------


instance Arbitrary Kind where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Direction where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Polarity where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Name where
  arbitrary = do
    x1 <- choose ('A', 'M')
    x2 <- elements "aeiou"
    x3 <- choose ('a', 'z')
    x4 <- choose ('a', 'z')
    return $ Name [x1, x2, x3, x4]

instance Arbitrary Param where
  arbitrary = do
    x1 <- choose ('x', 'z')
    x2 <- choose ('a', 'z')
    return $ Param [x1, x2]


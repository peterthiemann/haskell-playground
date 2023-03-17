{-# LANGUAGE LambdaCase, FlexibleContexts #-}
module Types where

import Data.String
import Test.QuickCheck
import PrettyShared

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
data Kind = SL | SU | TL | TU
  deriving (Eq, Ord, Show, Bounded, Enum)

subkind :: Kind -> Kind -> Bool
subkind SL SL = True
subkind SL TL = True
subkind SU SU = True
subkind SU SL = True
subkind SU TL = True
subkind SU TU = True
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
    TyVar { tyParam :: Param }
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

data Module = Module [Protocol] [Type]
  deriving (Eq, Show)

namedProtocol :: Protocol -> (Name, Protocol)
namedProtocol p = (prName p, p)

dualDirection :: Direction -> Direction
dualDirection Input = Output
dualDirection Output = Input

----------------------------------------------------------------------
-- substitution
----------------------------------------------------------------------

type Substitution = [(Param, TyProto)]

class Subst t where
  subst :: Substitution -> t -> t

instance Subst Type where
  subst s = \case
    TyVar parm -> TyVar parm
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
    TyApp pn pargs -> TyApp pn (map (subst s) pargs)

instance Subst Constructor where
  subst s = \case
    Constructor n args -> Constructor n (map (subst s) args)

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
    TyVar parm -> S.singleton parm
    TyUnit -> S.empty
    TyBase n -> S.empty
    TyLolli t1 t2 -> S.union (free t1) (free t2)
    TyArrow t1 t2 -> S.union (free t1) (free t2)
    TyPair  t1 t2 -> S.union (free t1) (free t2)
    TyPoly parm k ty -> free ty S.\\ S.singleton parm
    TySession ts -> free ts

instance Free TySession where
  free = \case
    SeVar parm -> S.singleton parm
    TyTransmit d tp tc -> S.union (free tp) (free tc)
    TyEnd d -> S.empty
    TyDual sy -> free sy

instance Free TyProto where
  free = \case
    TyType ty -> free ty
    TyApp pn pargs -> foldr S.union S.empty (map free pargs)

fresh :: Param -> S.Set Param -> Param
fresh def xs = head $ filter (not . flip S.member xs) (def : [ Param ("p" ++ show i) | i <- [0..] ])
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


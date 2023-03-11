module Types where

import Data.String

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
data Kind = SL | SU | TL | TU | ML | MU
  deriving (Eq, Ord, Show, Bounded, Enum)

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
  | TyDual { seBody :: Type }
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

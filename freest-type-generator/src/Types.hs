module Types where

-- type definitions for protocol definitions and types

type Name = String
type Param = String

data Polarity = Plus | Minus
  deriving (Eq, Ord, Show)
data Direction = Input | Output
  deriving (Eq, Ord, Show)
data Kind = SL | SU | TL | TU | ML | MU
  deriving (Eq, Ord, Show)

data Protocol =
  Protocol { prName :: Name
           , prParameters :: [Param]
           , prCtors :: [Constructor]
           }

data Constructor =
  Constructor { ctName :: Name
              , ctArgs :: [Argument]
              }

data Argument =
  Argument { argPolarity :: Polarity
           , argType :: Type
           }

data Type =
    TyVar { tyName :: Name }
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
  | TyPoly { tyName :: Name
           , tyKind :: Kind
           , tyBody :: Type
           }
  | TyApp { tyName :: Name
          , tyArgs :: [Type]
          }
  | TySession { tyDirection :: Direction
              , tyPayload :: Type
              , tyCont :: Type
              }
  | TyEnd { tyDirection :: Direction }
  | TyDual { tyBody :: Type }
  deriving (Eq, Ord, Show)

data Module = Module [Protocol] [Type]

  
namedProtocol :: Protocol -> (Name, Protocol)
namedProtocol p = (prName p, p)

dualDirection :: Direction -> Direction
dualDirection Input = Output
dualDirection Output = Input

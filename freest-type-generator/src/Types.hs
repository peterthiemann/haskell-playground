module Types where

import Data.String
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import Test.QuickCheck

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

subkind :: Kind -> Kind -> Bool
subkind SL SL = True
subkind SL ML = True
subkind SL TL = True
subkind SU SU = True
subkind SU SL = True
subkind SU TL = True
subkind SU TU = True
subkind SU MU = True
subkind SU ML = True
subkind TL TL = True
subkind TU TU = True
subkind TU TL = True
subkind ML ML = True
subkind MU MU = True
subkind MU ML = True
subkind ML TL = True
subkind MU TL = True
subkind MU TU = True
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
-- instances for PP
----------------------------------------------------------------------

instance Pretty Kind where
  pPrint = text . show

instance Pretty Direction where
  pPrint Input = text "?"
  pPrint Output = text "!"

instance Pretty Polarity where
  pPrint Plus = empty -- text "+"
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
    x1 <- choose ('X', 'Z')
    x2 <- choose ('A', 'Z')
    return $ Param [x1, x2]


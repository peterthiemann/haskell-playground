module Generators where

import Types

import Test.QuickCheck

-- number of protocol definitions
genNrProtocols :: Gen Int
genNrProtocols = choose (1, 1)

-- protocol parameters
genNrParameters :: Gen Int
genNrParameters = choose (0, 2)

-- number of constructors
genNrConstructors :: Gen Int
genNrConstructors = choose (1, 5)

-- number of constructor arguments
genNrArguments :: Gen Int
genNrArguments = choose (0, 10)

baseTypeNames :: [Name]
baseTypeNames = map Name ["Int", "Bool", "Char", "String"]


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

allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent (x : xs) = not (x `elem` xs) && allDifferent xs

genType :: Kind -> [(Param, Kind)] -> [Name] -> [Param] -> Gen Type
genType ofK tvenv pnames params = do
  let freqTyVar = case [ n | (n, k) <- tvenv, subkind k ofK ] of
        [] -> []
        tvnames ->
          [(1, pure TyVar <*> elements tvnames)]
      freqTyArrow
        | subkind TU ofK = 
          [(1, pure TyArrow <*> genType TL tvenv pnames params <*>  genType TL tvenv pnames params)]
        | otherwise = []
      freqTyPoly
        | subkind TU ofK = [
            (1, do tv <- arbitrary
                   ki <- arbitrary
                   pure (TyPoly tv ki) <*> genType ofK ((tv, ki) : tvenv) pnames params)]
        | otherwise = []
  frequency $ 
    freqTyVar ++ 
    freqTyArrow ++
    freqTyPoly ++ [
    (1, pure TyUnit),
    (1, pure TyBase <*> elements baseTypeNames),
    (1, pure TyLolli <*> genType TL tvenv pnames params <*>  genType TL tvenv pnames params),
    (1, pure TyPair  <*> genType ofK tvenv pnames params <*>  genType ofK tvenv pnames params),
    (1, pure TySession <*> genSession tvenv pnames params)
    ]

genSession :: [(Param, Kind)] -> [Name] -> [Param] -> Gen TySession
genSession tvenv pnames params = do
  let freqTyVar = case [ n | (n, k) <- tvenv, subkind k SL ] of
        [] -> []
        tvnames ->
          [(1, pure SeVar <*> elements tvnames)]
  frequency $
    freqTyVar ++ 
    [ (1, pure TyTransmit <*> arbitrary <*> genTyProto tvenv pnames params <*> genSession tvenv pnames params)
    , (1, pure TyEnd <*> arbitrary)
    -- , (1, pure TyDual <*> genSession tvenv pnames params)
    ]

genTyProto :: [(Param, Kind)] -> [Name] -> [Param] -> Gen TyProto
genTyProto tvenv pnames params = do
  frequency [(1, pure TyApp <*> elements pnames <*> pure (map (TyType . TyVar) params))
            ,(2, pure TyType <*> genType ML tvenv pnames params)
            ]

genArgument :: [(Param, Kind)] -> [Name] -> [Param] -> Gen Argument
genArgument tvenv pnames params = do
  pure Argument <*> arbitrary <*> genTyProto tvenv pnames params

genCtor :: [(Param, Kind)] -> [Name] -> [Param] -> Gen Constructor
genCtor tvenv pnames params = do
  cname <- arbitrary
  nrOfArgs <- genNrArguments
  args <- vectorOf nrOfArgs (genArgument tvenv pnames params)
  pure $ Constructor cname args

instance Arbitrary Protocol where
  arbitrary = do
    pname <- arbitrary
    nrOfCtors <- genNrConstructors
    nrOfParams <- genNrParameters
    params <- vectorOf nrOfParams arbitrary `suchThat` allDifferent
    ctors <- vectorOf nrOfCtors (genCtor [] [pname] params) `suchThat` (allDifferent . map ctName)
    pure (Protocol pname params ctors)
    
-- CONSTRUCTION --

-- instance Arbitrary Type where
--   arbitrary = pure TyUnit

-- instance Arbitrary TyProto where
--   arbitrary = pure TyType <*> arbitrary

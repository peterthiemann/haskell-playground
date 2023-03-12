module Generators
  (genType, genSession, genTyProto, genArgument, genCtor)
where

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
genNrConstructors = choose (1, 2)

-- number of constructor arguments
genNrArguments :: Gen Int
genNrArguments = choose (0, 10)

type Size = Int

baseTypeNames :: [Name]
baseTypeNames = map Name ["Int", "Bool", "Char", "String"]

allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent (x : xs) = not (x `elem` xs) && allDifferent xs

genType :: Size -> Kind -> [(Param, Kind)] -> [Name] -> [Param] -> Gen Type
genType size ofK tvenv pnames params = do
  let size2 = size `div` 2
      freqTyVar = case [ n | (n, k) <- tvenv, subkind k ofK ] of
        [] -> []
        tvnames ->
          [(1, pure TyVar <*> elements tvnames)]
      freqTyArrowUnitBasePair
        | subkind MU ofK = 
          [(size, pure TyArrow <*> genType size2 TL tvenv pnames params <*>  genType size2 TL tvenv pnames params)
          ,(1, pure TyUnit)
          ,(1, pure TyBase <*> elements baseTypeNames)
          ,(size, pure TyPair <*> genType size2 ofK tvenv pnames params <*>  genType size2 ofK tvenv pnames params)]
        | otherwise = []
      freqTyLolli
        | subkind ML ofK =
          [(size, pure TyLolli <*> genType size2 TL tvenv pnames params <*>  genType size2 TL tvenv pnames params)]
        | otherwise = []
      freqTyPoly
        | subkind MU ofK = [
            (size2, do tv <- arbitrary
                       ki <- arbitrary
                       pure (TyPoly tv ki) <*> genType size2 ofK ((tv, ki) : tvenv) pnames params)]
        | otherwise = []
      freqTySession
        | subkind SU ofK =
          [(2 * size + 1, pure TySession <*> genSession size tvenv pnames params)]
        | otherwise = []
  frequency $ 
    freqTyVar ++ 
    freqTyArrowUnitBasePair ++
    freqTyLolli ++
    freqTyPoly ++
    freqTySession

genSession :: Size -> [(Param, Kind)] -> [Name] -> [Param] -> Gen TySession
genSession size tvenv pnames params = do
  let size2 = size `div` 2
      freqTyVar = case [ n | (n, k) <- tvenv, subkind k SL ] of
        [] -> []
        tvnames ->
          [(1, pure SeVar <*> elements tvnames)]
  frequency $
    freqTyVar ++ 
    [ (size, pure TyTransmit <*> arbitrary <*> genTyProto size2 tvenv pnames params <*> genSession size2 tvenv pnames params)
    , (1, pure TyEnd <*> arbitrary)
    -- , (1, pure TyDual <*> genSession size tvenv pnames params)
    ]

genTyProto :: Size -> [(Param, Kind)] -> [Name] -> [Param] -> Gen TyProto
genTyProto size tvenv pnames params = do
  frequency [(1, pure TyApp <*> elements pnames <*> pure (map (TyType . TyVar) params))
            ,(2, pure TyType <*> genType size ML tvenv pnames params)
            ]

genArgument :: Size -> [(Param, Kind)] -> [Name] -> [Param] -> Gen Argument
genArgument size tvenv pnames params = do
  pure Argument <*> arbitrary <*> genTyProto size tvenv pnames params

genCtor :: Size -> [(Param, Kind)] -> [Name] -> [Param] -> Gen Constructor
genCtor size tvenv pnames params = do
  cname <- arbitrary
  nrOfArgs <- genNrArguments
  args <- vectorOf nrOfArgs (genArgument size tvenv pnames params)
  pure $ Constructor cname args

instance Arbitrary Protocol where
  arbitrary = do
    let size = 8
    pname <- arbitrary
    nrOfCtors <- genNrConstructors
    nrOfParams <- genNrParameters
    params <- vectorOf nrOfParams arbitrary `suchThat` allDifferent
    ctors <- vectorOf nrOfCtors (genCtor size [] [pname] params) `suchThat` (allDifferent . map ctName)
    pure (Protocol pname params ctors)
    
-- CONSTRUCTION --

-- instance Arbitrary Type where
--   arbitrary = pure TyUnit

-- instance Arbitrary TyProto where
--   arbitrary = pure TyType <*> arbitrary

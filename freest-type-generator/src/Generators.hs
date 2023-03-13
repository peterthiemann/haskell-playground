{-# LANGUAGE TupleSections #-}
module Generators
  (genType, genSession, genTyProto, genArgument, genCtor, genProtocol, genProtocols)
where

import Data.Maybe (fromJust)

import Types

import Test.QuickCheck

-- number of protocol definitions
genNrProtocols :: Gen Int
genNrProtocols = choose (1, 2)

-- protocol parameters
genNrParameters :: Gen Int
genNrParameters = choose (0, 2)

-- number of constructors
genNrConstructors :: Gen Int
genNrConstructors = choose (1, 2)

-- number of constructor arguments
genNrArguments :: Gen Int
genNrArguments = choose (0, 2)

type Size = Int

baseTypeNames :: [Name]
baseTypeNames = map Name ["Int", "Bool", "Char", "String"]

allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent (x : xs) = not (x `elem` xs) && allDifferent xs

genType :: Size -> Kind -> [(Param, Kind)] -> [(Name, [Param])] -> Gen Type
genType size ofK tvenv pnenv = do
  let size2 = size `div` 2
      freqTyVar = case [ n | (n, k) <- tvenv, subkind k ofK ] of
        [] -> []
        tvnames ->
          [(1, pure TyVar <*> elements tvnames)]
      freqTyArrowUnitBasePair
        | subkind MU ofK = 
          [(size, pure TyArrow <*> genType size2 TL tvenv pnenv <*>  genType size2 TL tvenv pnenv)
          ,(1, pure TyUnit)
          ,(1, pure TyBase <*> elements baseTypeNames)
          ,(size, pure TyPair <*> genType size2 ofK tvenv pnenv <*>  genType size2 ofK tvenv pnenv)]
        | otherwise = []
      freqTyLolli
        | subkind ML ofK =
          [(size, pure TyLolli <*> genType size2 TL tvenv pnenv <*>  genType size2 TL tvenv pnenv)]
        | otherwise = []
      freqTyPoly
        | subkind MU ofK = [
            (size2, do tv <- arbitrary
                       ki <- arbitrary
                       pure (TyPoly tv ki) <*> genType size2 ofK ((tv, ki) : tvenv) pnenv)]
        | otherwise = []
      freqTySession
        | subkind SU ofK =
          [(2 * size + 1, pure TySession <*> genSession size tvenv pnenv)]
        | otherwise = []
  frequency $ 
    freqTyVar ++ 
    freqTyArrowUnitBasePair ++
    freqTyLolli ++
    freqTyPoly ++
    freqTySession

genSession :: Size -> [(Param, Kind)] -> [(Name, [Param])] -> Gen TySession
genSession size tvenv pnenv = do
  let size2 = size `div` 2
      freqTyVar = case [ n | (n, k) <- tvenv, subkind k SL ] of
        [] -> []
        tvnames ->
          [(1, pure SeVar <*> elements tvnames)]
      freqTyEnd
        | null freqTyVar = [(1, pure TyEnd <*> arbitrary)]
        | otherwise  = []
  frequency $
    freqTyVar ++ freqTyEnd ++
    [ (size, pure TyTransmit <*> arbitrary <*> genTyProto size2 tvenv pnenv <*> genSession size2 tvenv pnenv)
    -- , (1, pure TyDual <*> genSession size tvenv pnames params)
    ]

genTyProto :: Size -> [(Param, Kind)] -> [(Name, [Param])] -> Gen TyProto
genTyProto size tvenv pnenv = do
  frequency [(1, do (pname, params) <- elements pnenv
                    pure (TyApp pname) <*> mapM (const (genTyProto size tvenv pnenv)) params)
            ,(2, pure TyType <*> genType size ML tvenv pnenv)
            ]

genArgument :: Size -> [(Param, Kind)] -> [(Name, [Param])] -> Gen Argument
genArgument size tvenv pnenv = do
  pure Argument <*> arbitrary <*> genTyProto size tvenv pnenv

genCtor :: Size -> [(Param, Kind)] -> [(Name, [Param])] -> Gen Constructor
genCtor size tvenv pnenv = do
  cname <- arbitrary
  nrOfArgs <- genNrArguments
  args <- vectorOf nrOfArgs (genArgument size tvenv pnenv)
  pure $ Constructor cname args

instance Arbitrary Protocol where
  arbitrary = do
    let size = 8
    pname <- arbitrary
    nrOfCtors <- genNrConstructors
    nrOfParams <- genNrParameters
    params <- vectorOf nrOfParams arbitrary `suchThat` allDifferent
    ctors <- vectorOf nrOfCtors (genCtor size [] [(pname, params)]) `suchThat` (allDifferent . map ctName)
    pure (Protocol pname params ctors)
    
genProtocol :: Size -> [(Name, [Param])] -> Name -> Gen Protocol
genProtocol size pnenv pname = do
  nrOfCtors <- genNrConstructors
  ctors <- vectorOf nrOfCtors (genCtor size [] pnenv) `suchThat` (allDifferent . map ctName)
  pure (Protocol pname (fromJust $ lookup pname pnenv) ctors)

genProtocols :: Size -> Gen [Protocol]
genProtocols size = do
  nrOfProtocols <- genNrProtocols
  pnames <- vectorOf nrOfProtocols arbitrary `suchThat` allDifferent
  nrOfParams <- genNrParameters
  params <- vectorOf nrOfParams arbitrary `suchThat` allDifferent
  mapM (genProtocol size (map (,params) pnames)) pnames

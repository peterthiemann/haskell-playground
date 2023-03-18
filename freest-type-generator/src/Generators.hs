{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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

baseTypeNames :: [Name]
baseTypeNames = map Name ["Int", "Char", "String", "()"]

allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent (x : xs) = x `notElem` xs && allDifferent xs

genType ::  Kind -> [(Param, Kind)] -> [(Name, [Param])] -> Gen Type
genType ofK tvenv pnenv = sized \size -> do
  let size2 = size `div` 2
      genType2 k tvenv' = resize size2 . genType k tvenv'
      freqTyVar = case [ n | (n, k) <- tvenv, subkind k ofK ] of
        [] -> []
        tvnames ->
          [(1, pure TyVar <*> elements tvnames)]
      freqTyArrowUnitBasePair
        | subkind TU ofK =
          [(size, pure TyArrow <*> genType2 TL tvenv pnenv <*>  genType2 TL tvenv pnenv)
          ,(1, pure TyUnit)
          ,(1, pure TyBase <*> elements baseTypeNames)
          ,(size, pure TyPair <*> genType2 ofK tvenv pnenv <*>  genType2 ofK tvenv pnenv)]
        | otherwise = []
      freqTyLolli
        | subkind TL ofK =
          [(size, pure TyLolli <*> genType2 TL tvenv pnenv <*>  genType2 TL tvenv pnenv)]
        | otherwise = []
      freqTyPoly
        | subkind TU ofK = [
            (size2, do tv <- arbitrary
                       ki <- arbitrary
                       pure (TyPoly tv ki) <*> genType2 ofK ((tv, ki) : tvenv) pnenv)]
        | otherwise = []
      freqTySession
        | subkind SU ofK =
          [(2 * size + 1, pure TySession <*> genSession tvenv pnenv)]
        | otherwise = []
  frequency $
    freqTyVar ++
    freqTyArrowUnitBasePair ++
    freqTyLolli ++
    freqTyPoly ++
    freqTySession

genSession :: [(Param, Kind)] -> [(Name, [Param])] -> Gen TySession
genSession tvenv pnenv = sized \size -> do
  let halfSized = resize (size `div` 2)
      freqTyVar = case [ n | (n, k) <- tvenv, subkind k SL ] of
        [] -> []
        tvnames ->
          [(1, pure SeVar <*> elements tvnames)]
      freqTyEnd
        | null freqTyVar = [(1, pure TyEnd <*> arbitrary)]
        | otherwise  = []
  frequency $
    freqTyVar ++ freqTyEnd ++
    [ (size, pure TyTransmit <*> arbitrary <*> halfSized (genTyProto tvenv pnenv) <*> halfSized (genSession tvenv pnenv))
    -- , (1, pure TyDual <*> genSession size tvenv pnames params)
    ]

genTyProto :: [(Param, Kind)] -> [(Name, [Param])] -> Gen TyProto
genTyProto tvenv pnenv = do
  frequency [(1, do (pname, params) <- elements pnenv
                    pure (TyApp pname) <*> mapM (const (genTyProto tvenv pnenv)) params)
            ,(2, pure TyType <*> genType TL tvenv pnenv)
            ]

genArgument :: [(Param, Kind)] -> [(Name, [Param])] -> Gen Argument
genArgument tvenv pnenv = do
  pure Argument <*> arbitrary <*> genTyProto tvenv pnenv

genCtor :: [(Param, Kind)] -> [(Name, [Param])] -> Gen Constructor
genCtor tvenv pnenv = do
  cname <- arbitrary
  nrOfArgs <- genNrArguments
  args <- vectorOf nrOfArgs (genArgument tvenv pnenv)
  pure $ Constructor cname args

instance Arbitrary Protocol where
  arbitrary = do
    pname <- arbitrary
    nrOfCtors <- genNrConstructors
    nrOfParams <- genNrParameters
    params <- vectorOf nrOfParams arbitrary `suchThat` allDifferent
    ctors <- vectorOf nrOfCtors (genCtor [] [(pname, params)]) `suchThat` (allDifferent . map ctName)
    pure (Protocol pname params ctors)

genProtocol :: [(Name, [Param])] -> Name -> Gen Protocol
genProtocol pnenv pname = do
  nrOfCtors <- genNrConstructors
  ctors <- vectorOf nrOfCtors (genCtor [] pnenv) `suchThat` (allDifferent . map ctName)
  pure (Protocol pname (fromJust $ lookup pname pnenv) ctors)

genProtocols :: Gen [Protocol]
genProtocols = do
  nrOfProtocols <- genNrProtocols
  pnames <- vectorOf nrOfProtocols arbitrary `suchThat` allDifferent
  nrOfParams <- genNrParameters
  params <- vectorOf nrOfParams arbitrary `suchThat` allDifferent
  mapM (genProtocol (map (,params) pnames)) pnames

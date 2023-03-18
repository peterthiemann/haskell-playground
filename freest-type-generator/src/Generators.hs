{-# LANGUAGE BlockArguments #-}
module Generators
  (genType, genSession, genTyProto, genArgument, genCtor, genProtocol, genProtocols)
where

import Types

import Test.QuickCheck
import Control.Monad
import Data.Functor

-- number of protocol definitions
genNrProtocols :: Gen Int
genNrProtocols = choose (1, 2)

-- protocol parameters
genNrParameters :: Gen Int
genNrParameters = choose (0, 2)

baseTypeNames :: [Name]
baseTypeNames = map Name ["Int", "Char", "String", "()"]

allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent (x : xs) = x `notElem` xs && allDifferent xs

genType :: Kind -> [(Param, Kind)] -> [Protocol] -> Gen Type
genType ofK tvenv pnenv = sized \size -> do
  let size2 = size `div` 2
      genType2 k tvenv' = resize size2 . genType k tvenv'
      freqTyVar = case [ n | (n, k) <- tvenv, subkind k ofK ] of
        [] -> []
        tvnames ->
          [(1, pure TyVar <*> elements tvnames)]
      freqTyArrowUnitBasePair
        | subkind TU ofK =
          [(size, pure TyArrow <*> genType2 TL tvenv pnenv <*> genType2 TL tvenv pnenv)
          ,(1, pure TyUnit)
          ,(1, pure TyBase <*> elements baseTypeNames)
          ,(size, pure TyPair <*> genType2 ofK tvenv pnenv <*> genType2 ofK tvenv pnenv)]
        | otherwise = []
      freqTyLolli
        | subkind TL ofK =
          [(size, pure TyLolli <*> genType2 TL tvenv pnenv <*> genType2 TL tvenv pnenv)]
        | otherwise = []
      freqTyPoly
        | subkind TU ofK = [
            (size2, do tv <- arbitrary
                       ki <- arbitrary
                       pure (TyPoly tv ki) <*> genType2 ofK ((tv, ki) : tvenv) pnenv)]
        | otherwise = []
      freqTySession
        | subkind SL ofK =
          [(2 * size + 1, pure TySession <*> genSession tvenv pnenv)]
        | otherwise = []
  frequency $
    freqTyVar ++
    freqTyArrowUnitBasePair ++
    freqTyLolli ++
    freqTyPoly ++
    freqTySession

genSession :: [(Param, Kind)] -> [Protocol] -> Gen TySession
genSession tvenv pnenv = do
  params <- sizedParams <&> \p -> p `withMode` 3
  steps <- genListOf params stepGen
  stop <- stopGen
  pure $ foldr ($) stop steps
  where
    stepGen = TyTransmit <$> arbitrary <*> genTyProto tvenv pnenv
    stopGen = case [ n | (n, k) <- tvenv, subkind k SL ] of
      [] -> TyEnd <$> arbitrary
      names -> SeVar <$> elements names

genTyProto :: [(Param, Kind)] -> [Protocol] -> Gen TyProto
genTyProto tvenv pnenv = sized \sz -> frequency
  [ (sz,
        do p <- elements pnenv
           sizes <- genSizePartitionN $ length $ prParameters p
           args <- eachSize sizes (genTyProto tvenv pnenv)
           pure $ TyApp (prName p) args)
  , (2, pure TyType <*> genType TL tvenv pnenv)
  ]

genArgument :: [(Param, Kind)] -> [Protocol] -> Gen Argument
genArgument tvenv pnenv = do
  Argument <$> arbitrary <*> genTyProto tvenv pnenv

genCtor :: [(Param, Kind)] -> [Protocol] -> Gen Constructor
genCtor tvenv pnenv = do
  lps <- sized \n -> pure (0, min n 2, min n 4)
  Constructor <$> arbitrary <*> genListOf lps (genArgument tvenv pnenv)

genProtocol :: [Protocol] -> Gen [Constructor]
genProtocol pnenv =
  genListOf (1, 2, 4) (genCtor [] pnenv) `suchThat` (allDifferent . map ctName)

genProtocols :: Gen [Protocol]
genProtocols = do
  nrOfProtocols <- genNrProtocols
  protos <- vectorOf nrOfProtocols genProto `suchThat` (allDifferent . map prName)
  mapM (completeProto protos) protos `suchThat` (allDifferent . map ctName . concatMap prCtors)
  where
    genProto = do
      name <- arbitrary
      nrOfParams <- genNrParameters
      params <- vectorOf nrOfParams arbitrary `suchThat` allDifferent
      pure $ Protocol name params []
    completeProto penv p = do
      ctors <- genProtocol penv
      pure p{ prCtors = ctors }

-------------------------------------------------------------------------------
-- The below code is adapted from genvalidity:
-- https://hackage.haskell.org/package/genvalidity-1.1.0.0
--
-- It's goal is to distribute the available generator size between the
-- different generated list elements such that the sizes of the parts sum to
-- the generators size.
--
-- In a change from genvalidity the code is parameterized by `ListParams`,
-- parameters to the triangular distribution used by `genListLength`.
-- (genvalidity always uses a minimum of 0, mode of 2, and maximum of the
-- current size).

-- | Parameters to the triangular distribution: min, mode, max. It should hold
-- that @min ≤ mode ≤ max@.
type ListParams = (Int, Int, Int)

-- | Constructs 'ListParams' using a minimum size of 0, a mode of 2 and a
-- maximum of the current size. If the current size is less than 2 the mode is
-- lowered accordingly.
sizedParams :: Gen ListParams
sizedParams = sized \n -> pure (0, min n 2, n)

{-
-- | Adjusts 'ListParams' minimum whilst ensuring the 'ListParams' invariant (by
-- adjusting mode and max upwards).
withMin :: ListParams -> Int -> ListParams
withMin (_, pmode, pmax) newMin = (newMin, max newMin pmode, max newMin pmax)
-}

-- | Adjusts 'ListParams' mode whilst ensuring the 'ListParams' invariant (by
-- not changing the mode to much).
withMode :: ListParams -> Int -> ListParams
withMode (pmin, _, pmax) newMode = (pmin, min (max pmin newMode) pmax, pmax)

genListOf :: ListParams -> Gen a -> Gen [a]
genListOf params g = do
  sizes <- genSizePartition params
  eachSize sizes g

eachSize :: [Int] -> Gen a -> Gen [a]
eachSize sizes g = traverse (\sz -> resize sz g) sizes

genSizePartition :: ListParams -> Gen [Int]
genSizePartition params = genListLength params >>= genSizePartitionN

genSizePartitionN :: Int -> Gen [Int]
genSizePartitionN len = sized \size -> do
  us <- replicateM len $ choose (0, 1)
  let invs = map (invE 0.25) us
  -- Rescale the sizes to (approximately) sum to the given size.
  pure $ map (round . (* (fromIntegral size / sum invs))) invs
  where
    -- Use an exponential distribution for generating the
    -- sizes in the partition.
    invE :: Double -> Double -> Double
    invE lambda u = (-log (1 - u)) / lambda

genListLength ::  ListParams -> Gen Int
genListLength (pmin, pmode, pmax) =
  round . invT <$> choose (0, 1)
  where
    -- Use a triangle distribution for generating the length of the list.
    invT :: Double -> Double
    invT u =
      let a = fromIntegral pmin
          b = fromIntegral pmax
          c = fromIntegral pmode
          fc = (c - a) / (b - a)
       in if u < fc
            then a + sqrt (u * (b - a) * (c - a))
            else b - sqrt ((1 - u) * (b - a) * (b - c))

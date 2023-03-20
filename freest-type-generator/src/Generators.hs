{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Generators
  ( genType,
    genSession,
    genTyProto,
    genArgument,
    genCtor,
    genProtocol,
    genProtocols,
    Variations (..),
    genVariant,
  )
where

import Control.Monad
import Data.Bifunctor
import Data.Functor
import Data.Map qualified as M
import Data.Set qualified as S
import Test.QuickCheck
import Types

-- number of protocol definitions
genNrProtocols :: Gen Int
genNrProtocols = choose (1, 3)

-- protocol parameters
genNrParameters :: Gen Int
genNrParameters = pure 0

baseTypeNames :: [Name]
baseTypeNames = map Name ["Int", "Char", "String"]

allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent (x : xs) = x `notElem` xs && allDifferent xs

genType :: Kind -> [(Two Param, Kind)] -> [Protocol] -> Gen (Two Type)
genType ofK tvenv pnenv = sized \size -> do
  (a, b) <- decrSize splitSize
  let sizedA = resize a
      sizedB = resize b

  let tyvar :: [Two Type]
      tyvar = [TyVar <$> n | (n, k) <- tvenv, k `subkind` ofK]

  let baseTy :: [Two Type]
      baseTy = Two TyUnit TyUnit : [pure (TyBase n) | n <- baseTypeNames]

  let pairTy :: Gen (Two Type)
      pairTy = do
        t <- sizedA (genType ofK tvenv pnenv)
        u <- sizedB (genType ofK tvenv pnenv)
        pure $ TyPair <$> t <*> u

  let arrowTy :: (Type -> Type -> Type) -> Gen (Two Type)
      arrowTy con = do
        t <- sizedA (genType TL tvenv pnenv)
        u <- sizedB (genType TL tvenv pnenv)
        pure $ con <$> t <*> u

  let polyTy :: Gen (Two Type)
      polyTy = do
        let v1 =
              arbitrary `suchThat` \x ->
                x `notElem` [v | (Two v _, _) <- tvenv]
        let v2 =
              arbitrary `suchThat` \x ->
                x `notElem` [v | (Two _ v, _) <- tvenv]
        tv <- Two <$> v1 <*> v2
        k <- arbitrary
        t <- decrSize $ genType TL ((tv, k) : tvenv) pnenv
        pure $ TyPoly <$> tv <*> pure k <*> t

  let sessionTy :: Gen (Two Type)
      sessionTy = do
        t <- genSession tvenv pnenv
        pure $ TySession <$> t

  let ifSubkind k' g
        | k' `subkind` ofK = [g]
        | otherwise = []

  if size <= 1
    then do
      elements (tyvar ++ baseTy)
    else do
      oneof $
        ifSubkind TU pairTy
          ++ ifSubkind TU polyTy
          ++ ifSubkind TU (arrowTy TyArrow)
          ++ ifSubkind TL (arrowTy TyLolli)
          ++ ifSubkind SL sessionTy

decrSize :: Gen a -> Gen a
decrSize = scale \n -> max 0 (n - 1)

splitSize :: Gen (Int, Int)
splitSize = sized \size -> do
  a <- chooseInt (0, size)
  pure (a, size - a)

genSession :: [(Two Param, Kind)] -> [Protocol] -> Gen (Two TySession)
genSession tvenv pnenv = do
  params <- sizedParams <&> \p -> p `withMode` 4
  sizes <- genSizePartition params
  go False sizes
  where
    go :: Bool -> [Int] -> Gen (Two TySession)
    go pendingDual [] = do
      let endChoices = pure . TyEnd <$> arbitrary
      let varChoices = [pure (SeVar <$> n) | (n, k) <- tvenv, subkind k SL]
      Two end1 end2 <- oneof (endChoices : varChoices)
      pure $ Two end1 $ if pendingDual then TyDual end2 else end2
    go pendingDual0 (sz0 : szs) = do
      (wrap, pendingDual, sz) <- elements do
        (id, pendingDual0, sz0) : [(TyDual, not pendingDual0, sz0 - 1) | sz0 > 1]
      transmit <- genTransmit pendingDual
      payload <- resize sz (genTyProto tvenv pnenv)
      rest <-
        oneof
          [ go pendingDual szs,
            go (not pendingDual) szs <&> \(Two a b) -> Two a (TyDual b)
          ]
      pure $ Two wrap id <*> (transmit <*> payload <*> rest)

    genTransmit :: Bool -> Gen (Two (TyProto -> TySession -> TySession))
    genTransmit pendingDual =
      oneof
        [ do
            d <- arbitrary
            let d' = if pendingDual then dualDirection d else d
            pure $ Two (TyTransmit d) (TyTransmit d'),
          do
            d <- arbitrary
            let d' = if pendingDual then dualDirection d else d
            pure $ Two (TyTransmit d) (TyTransmit (dualDirection d') . TyNeg)
        ]

genTyProto :: [(Two Param, Kind)] -> [Protocol] -> Gen (Two TyProto)
genTyProto tvenv pnenv = sized \size -> do
  let pureTy = do
        t <- genType TL tvenv pnenv
        pure $ TyType <$> t
  let negTy = do
        t <- decrSize $ genTyProto tvenv pnenv
        pure $ TyNeg <$> t
  let appTy = do
        p <- elements pnenv
        sizes <- decrSize $ genSizePartitionN $ length $ prParameters p
        args <- sequenceA <$> eachSize sizes (genTyProto tvenv pnenv)
        pure $ TyApp (prName p) <$> args
  frequency . concat $
    [ [(1, pureTy)],
      [(3, pureTy) | size > 1],
      [(1, negTy) | size > 1],
      [(1, appTy) | not (null pnenv)]
    ]

class Variations a where
  listVariations :: M.Map Param Kind -> a -> [a]

genVariant :: Variations a => a -> Gen a
genVariant = elements . listVariations M.empty

instance Variations Type where
  listVariations !vs ty =
    baseTypeVariation ty ++ case ty of
      TyVar _ -> []
      TyUnit -> []
      TyBase _ -> []
      TyLolli t u -> arrowVariation TyLolli TyArrow t u
      TyArrow t u -> arrowVariation TyArrow TyLolli t u
      TyPair t u -> t : u : [TyPair t' u | t' <- listVariations vs t] ++ [TyPair t u' | u' <- listVariations vs u]
      TyPoly p k t -> [t | p `S.notMember` free t] ++ [TyPoly p k t' | t' <- listVariations (M.insert p k vs) t]
      TySession s -> [TySession s' | s' <- listVariations vs s]
    where
      baseTypeVariation t =
        filter (t /=) $ TyUnit : fmap TyBase baseTypeNames ++ [TyVar v | (v, k) <- M.toList vs, TU `subkind` k]
      arrowVariation a1 a2 t u =
        a2 t u : [a1 t' u | t' <- listVariations vs t] ++ [a1 t u' | u' <- listVariations vs u]

instance Variations TySession where
  listVariations vs = go
    where
      baseVariations t =
        filter (t /=) $ [SeVar v | (v, k) <- M.toList vs, SL `subkind` k]
      go ty =
        baseVariations ty ++ case ty of
          SeVar _ -> []
          TyTransmit d p s -> TyTransmit (dualDirection d) p s : s : [TyTransmit d p s' | s' <- go s] ++ [TyTransmit d p' s | p' <- listVariations vs p]
          TyEnd _ -> [] -- Don't flip the direction here, as this difference does not translate to FreeST.
          TyDual s -> s : [TyDual s' | s' <- go s]

instance Variations TyProto where
  listVariations vs = go
    where
      go = \case
        TyType t -> [TyType t' | t' <- listVariations vs t]
        TyPVar _ -> [] -- Not generated.
        TyNeg t -> t : [TyNeg t' | t' <- go t]
        TyApp {} -> [] -- Simplification.

genArgument :: [(Param, Kind)] -> [Protocol] -> Gen Argument
genArgument tvenv pnenv = Argument <$> arbitrary <*> genTy
  where
    genTy = genTyProto (first pure <$> tvenv) pnenv <&> \(Two a _) -> a

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
      pure p {prCtors = ctors}

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

genListLength :: ListParams -> Gen Int
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

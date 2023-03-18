{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module GenExamples (runGenerator, GenConfig(..)) where

import Data.Either (partitionEithers)
import Data.Foldable
import Data.List (intercalate)
import System.Exit
import System.FilePath
import System.IO
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

import Examples
import Generators
import PrettyAlgST qualified as PA
import PrettyFreeST qualified as PF
import PrettyShared
import Types

data GenConfig = GenConfig
  { psize :: Int
  , tsize :: Int
  , pseed :: Maybe Int
  , tseed :: Maybe Int
  , toolbox :: Bool
  , outputFile :: Maybe FilePath
  , protocols :: [String]
  }

protocolEnvironment :: [Protocol]
protocolEnvironment =  [pIntListP, pListP, pArith, pStream, pSeq, pEither, pRepeat]

toolboxEnvironment :: [Protocol]
toolboxEnvironment = [pSeq, pEither, pRepeat]

resolveSeed :: Maybe Int -> IO Int
resolveSeed = maybe randomIO pure

runGenerator :: GenConfig -> IO ()
runGenerator config = do
  -- Generate protocols/select protocols.
  newPSeed <- resolveSeed (pseed config)
  newTSeed <- resolveSeed (tseed config)
  putStrLn "--- Rerun with ---"
  putStrLn $ "--pseed=" ++ show newPSeed
  putStrLn $ "--tseed=" ++ show newTSeed
  putStrLn $ "--psize=" ++ show (psize config)
  putStrLn $ "--tsize=" ++ show (tsize config)
  ps <-
    if | null (protocols config) && toolbox config ->
           pure toolboxEnvironment
       | null (protocols config) ->
           generateWithSeed newPSeed $ genProtocols (psize config)
       | toolbox config ->
           selectProtocols (protocols config) toolboxEnvironment
       | otherwise ->
           selectProtocols (protocols config) protocolEnvironment
  let pnenv = map (\p -> (prName p, prParameters p)) ps
  -- let pnames = map prName ps
  --     params = head (map prParameters ps)
  t <- generateWithSeed newTSeed $ genType (tsize config) TL [] pnenv
  let m = Module ps [t]
  let algstDoc = PA.runPretty $ PA.prettyModule m
  let freestDoc = PF.runPretty $ PF.prettyModule m
  case outputFile config of
    Nothing -> do
      putStrLn "--- protocol and type in AlgST syntax ---"
      putDoc algstDoc
      putStrLn "-----------------------------------------"
      putStrLn "--- corresponding type in FreeST syntax ---"
      putDoc freestDoc
    Just basePath -> do
      let algstPath = basePath <.> "algst"
      let freestPath = basePath <.> "fst"
      putStrLn $ "--- writing protocols and types in AlgST syntax to " ++ algstPath
      withFile algstPath WriteMode (`hPutDoc` algstDoc)
      putStrLn $ "--- writing corresponding types in FreeST syntax to " ++ freestPath
      withFile freestPath WriteMode (`hPutDoc` freestDoc)

generateWithSeed :: Int -> Gen a -> IO a
generateWithSeed seed gena =
  do let r = mkQCGen seed
     return (unGen gena r 30)

selectProtocols :: [String] -> [Protocol] -> IO [Protocol]
selectProtocols names penv =
  case partitionEithers (lookupProtocol <$> names) of
    ([], ps) -> pure ps
    (notFound, _) -> do
      for_ notFound \name ->
        hPutStrLn stderr $ "unknown protocol " ++ name
      hPutStrLn stderr $ "known protocols: " ++
        intercalate ", " [ n | Protocol { prName = Name n } <- penv ]
      exitFailure
  where
    lookupProtocol name =
      maybe (Left name) Right $
        lookup (Name name) (map namedProtocol penv)

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module GenExamples (runGenerator, GenConfig(..)) where

import Control.Monad
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
  , count :: Int
  , negative :: Bool
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

type RerunInfo = (Int, Int, GenConfig)

putRerunInfo :: Handle -> RerunInfo -> IO ()
putRerunInfo h (ps, ts, config) = do
  hPutStrLn h "--- Rerun with ---"
  hPutStrLn h $ "--pseed=" ++ show ps
  hPutStrLn h $ "--tseed=" ++ show ts
  hPutStrLn h $ "--psize=" ++ show (psize config)
  hPutStrLn h $ "--tsize=" ++ show (tsize config)
  hPutStrLn h $ "--count=" ++ show (count config)
  when (negative config) do
    hPutStrLn h "--negative"
  hPutStrLn h "------------------"
  hPutStrLn h ""

runGenerator :: GenConfig -> IO ()
runGenerator config = do
  -- Generate protocols/select protocols.
  newPSeed <- resolveSeed (pseed config)
  newTSeed <- resolveSeed (tseed config)
  let rerun = (newPSeed, newTSeed, config)
  putRerunInfo stdout rerun
  pnenv <-
    if | null (protocols config) && toolbox config ->
           pure toolboxEnvironment
       | null (protocols config) ->
           generateWithSeed newPSeed $ resize (psize config) genProtocols
       | toolbox config ->
           selectProtocols (protocols config) toolboxEnvironment
       | otherwise ->
           selectProtocols (protocols config) protocolEnvironment
  ts <- generateWithSeed newTSeed $ replicateM (count config) $ resize (tsize config) do
    Two t u <- genType SL [] pnenv
    Two t <$> if negative config then genVariant u else pure u
  let m = Module pnenv ts
  let algstDoc = PA.runPretty $ PA.prettyModule (not (negative config)) m
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
      withFile algstPath WriteMode \h ->
        putRerunInfo h rerun >> hPutDoc h algstDoc
      putStrLn $ "--- writing corresponding types in FreeST syntax to " ++ freestPath
      withFile freestPath WriteMode \h ->
        putRerunInfo h rerun >> hPutDoc h freestDoc

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

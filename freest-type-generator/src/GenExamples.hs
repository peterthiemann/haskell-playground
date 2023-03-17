{-# LANGUAGE OverloadedStrings #-}
module GenExamples (runGenerator, GenConfig(..)) where

import System.Random
import Test.QuickCheck
import Types
import Generators

import qualified PrettyFreeST as PF
import qualified PrettyAlgST as PA

import Examples

-- for repeatable random number generation

import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

data GenConfig = GenConfig
  { psize :: Int
  , tsize :: Int
  , pseed :: Int
  , tseed :: Int
  , toolbox :: Bool
  , repeatable :: Bool
  , protocols :: [String]
  }


protocolEnvironment :: [Protocol]
protocolEnvironment =  [pIntListP, pListP, pArith, pStream, pSeq, pEither, pRepeat]

runGenerator :: GenConfig -> IO ()
runGenerator config
  | repeatable config = do
    newTseed <- if tseed config /= -1
                then pure (tseed config)
                else randomIO
    newPseed <- if pseed config /= -1
                then pure (pseed config)
                else randomIO
    let newconfig = config { tseed = newTseed, pseed = newPseed }
    runRepeatable newconfig
  | otherwise =
    if toolbox config
    then withToolbox config
    else randomProtocols config

randomProtocols :: GenConfig -> IO ()
randomProtocols config = do
  ps <- generate $ genProtocols (psize config)
  withProtocols ps config

withProtocols :: [Protocol] -> GenConfig -> IO ()
withProtocols ps config = do
  let pnenv  = map (\p -> (prName p, prParameters p)) ps
      -- pnames = map prName ps
      -- params = head (map prParameters ps)
  t <- generate $ genType (tsize config) TL [] pnenv
  let m = Module ps [t]
  putStrLn "--- protocol and type in AlgST syntax ---"
  PA.putPretty $ PA.prettyModule m
  putStrLn "-----------------------------------------"
  putStrLn "--- corresponding type in FreeST syntax ---"
  PF.putPretty $ PF.prettyModule m

withToolbox :: GenConfig -> IO ()
withToolbox = withSelectedProtocols ["Seq", "Either", "Repeat"]

withSelectedProtocols :: [Name] -> GenConfig -> IO ()
withSelectedProtocols pnames =
  withProtocols [ p | pn <- pnames, Just p <- [lookup pn (map namedProtocol protocolEnvironment)]]

generateWithSeed :: Int -> Gen a -> IO a
generateWithSeed seed gena =
  do let r = mkQCGen seed
     return (unGen gena r 30)

runRepeatable :: GenConfig -> IO ()
runRepeatable conf = do
  putStrLn "--- generating with configuration ---"
  putStrLn ("--pseed=" ++ show (pseed conf))
  putStrLn ("--tseed=" ++ show (tseed conf))
  putStrLn ("--psize=" ++ show (psize conf))
  putStrLn ("--tsize=" ++ show (tsize conf))
  ps <- generateWithSeed (pseed conf) $ genProtocols (psize conf)
  let pnenv  = map (\p -> (prName p, prParameters p)) ps
  -- let pnames = map prName ps
  --     params = head (map prParameters ps)
  t <- generateWithSeed (tseed conf) $ genType (tsize conf) TL [] pnenv
  let m = Module ps [t]
  putStrLn "--- protocol and type in AlgST syntax ---"
  PA.putPretty $ PA.prettyModule m
  putStrLn "-----------------------------------------"
  putStrLn "--- corresponding type in FreeST syntax ---"
  PF.putPretty $ PF.prettyModule m

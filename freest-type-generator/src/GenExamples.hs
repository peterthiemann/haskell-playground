{-# LANGUAGE OverloadedStrings #-}
module GenExamples (example, repeatable, GenConfig(..)) where

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
  , toolbox :: Bool
  , protocols :: [String]
  }


protocolEnvironment :: [Protocol]
protocolEnvironment =  [pIntListP, pListP, pArith, pStream, pSeq, pEither, pRepeat]

example :: GenConfig -> IO ()
example config =
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
  putStrLn $ PA.pretty $ PA.prettyModule m
  putStrLn "-----------------------------------------"
  putStrLn "--- corresponding type in FreeST syntax ---"
  putStrLn $ PF.pretty $ PF.prettyModule m

withToolbox :: GenConfig -> IO ()
withToolbox = withSelectedProtocols ["Seq", "Either", "Repeat"]

withSelectedProtocols :: [Name] -> GenConfig -> IO ()
withSelectedProtocols pnames =
  withProtocols [ p | pn <- pnames, Just p <- [lookup pn (map namedProtocol protocolEnvironment)]]

generateWithSeed :: Int -> Gen a -> IO a
generateWithSeed seed gena =
  do let r = mkQCGen seed
     return (unGen gena r 30)

repeatable :: Int -> Int -> Int -> Int -> IO ()
repeatable tseed pseed tsize psize = do
  ps <- generateWithSeed pseed $ genProtocols psize
  let pnenv  = map (\p -> (prName p, prParameters p)) ps
  -- let pnames = map prName ps
  --     params = head (map prParameters ps)
  t <- generateWithSeed tseed $ genType tsize TL [] pnenv
  let m = Module ps [t]
  putStrLn "--- protocol and type in AlgST syntax ---"
  putStrLn $ PA.pretty $ PA.prettyModule m
  putStrLn "-----------------------------------------"
  putStrLn "--- corresponding type in FreeST syntax ---"
  putStrLn $ PF.pretty $ PF.prettyModule m
  

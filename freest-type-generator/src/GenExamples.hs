{-# LANGUAGE OverloadedStrings #-}
module GenExamples (example, repeatable) where

import Test.QuickCheck
import Types
import Generators

import qualified PrettyFreeST as PF
import qualified PrettyAlgST as PA

import Examples

-- for repeatable random number generation

import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

protocolEnvironment :: [Protocol]
protocolEnvironment =  [pIntListP, pListP, pArith, pStream, pSeq, pEither, pRepeat]

example :: IO ()
example = withToolbox

randomProtocols :: IO ()
randomProtocols = do
  let psize = 8
  ps <- generate $ genProtocols psize
  withProtocols ps

withProtocols :: [Protocol] -> IO ()
withProtocols ps = do
  let pnenv  = map (\p -> (prName p, prParameters p)) ps
      -- pnames = map prName ps
      -- params = head (map prParameters ps)
      tsize  = 32
  t <- generate $ genType tsize TL [] pnenv
  let m = Module ps [t]
  putStrLn "--- protocol and type in AlgST syntax ---"
  putStrLn $ PA.pretty $ PA.prettyModule m
  putStrLn "-----------------------------------------"
  putStrLn "--- corresponding type in FreeST syntax ---"
  putStrLn $ PF.pretty $ PF.prettyModule m

withToolbox :: IO ()
withToolbox = withSelectedProtocols ["Seq", "Either", "Repeat"]

withSelectedProtocols :: [Name] -> IO ()
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
  

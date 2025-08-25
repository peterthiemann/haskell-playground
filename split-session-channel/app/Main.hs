module Main (main) where

import Criterion.Main
import System.Environment (getArgs)
import qualified Benchme as B

repeats :: Int
repeats = 200

main_orig :: IO ()
main_orig = do
  args <- getArgs
  case args of
    ["Cell"] ->
      B.bench B.onerunLib repeats
    ["Split"] ->
      B.bench B.onerunSplit repeats
    ["Prim"] ->
      B.bench B.onerunPrim repeats
    _ ->
      putStrLn ("Usage: argument `Cell` selects cell-based implementation thread local, `Split` selects cell-based implementation with splits at each operations, `Prim` selects primitive implementation of channels")

-- Our benchmark harness.
main = defaultMain [
  bgroup "channels" [ bench "Prim"  $ whnfIO (B.bench B.onerunPrim repeats)
               , bench "Splt"  $ whnfIO (B.bench B.onerunSplit repeats)
               , bench "Cell"  $ whnfIO (B.bench B.onerunLib repeats)
               ]
  ]


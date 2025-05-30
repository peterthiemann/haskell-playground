module Main (main) where

import System.Environment (getArgs)
import Benchme (bench, onerunLib, onerunPrim, onerunSplit)

repeats :: Int
repeats = 200

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["Cell"] ->
      bench onerunLib repeats
    ["Split"] ->
      bench onerunSplit repeats
    ["Prim"] ->
      bench onerunPrim repeats
    _ ->
      putStrLn ("Usage: argument `Cell` selects cell-based implementation, `Prim` selects primitive implementation of channels")


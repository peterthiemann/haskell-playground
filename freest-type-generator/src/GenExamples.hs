module GenExamples (example) where

import Test.QuickCheck
import Types
import Generators

import qualified PrettyFreeST as PF
import qualified PrettyAlgST as PA

example :: IO ()
example = do
  let size = 32
  p@(Protocol pn ps _cs) <- generate arbitrary
  t <- generate $ genType size SL [] [pn] ps
  let m = Module [p] [t]
  putStrLn "--- protocol and type in AlgST syntax ---"
  putStrLn $ PA.pretty $ PA.prettyModule m
  putStrLn "-----------------------------------------"
  putStrLn "--- corresponding type in FreeST syntax ---"
  putStrLn $ PF.pretty $ PF.prettyModule m

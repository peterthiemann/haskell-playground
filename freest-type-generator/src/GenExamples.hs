module GenExamples (example) where

import Test.QuickCheck
import Types
import Generators

import qualified PrettyFreeST as PF
import qualified PrettyAlgST as PA

example :: IO ()
example = do
  let tsize = 32
      psize = 8
  ps <- generate $ genProtocols psize
  let pnames = map prName ps
      params = head (map prParameters ps)
  t <- generate $ genType tsize TL [] pnames params
  let m = Module ps [t]
  putStrLn "--- protocol and type in AlgST syntax ---"
  putStrLn $ PA.pretty $ PA.prettyModule m
  putStrLn "-----------------------------------------"
  putStrLn "--- corresponding type in FreeST syntax ---"
  putStrLn $ PF.pretty $ PF.prettyModule m

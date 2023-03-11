module Generators where

import Types

import Test.QuickCheck

maxConstructors :: Integer
maxConstructors = 5

instance Arbitrary Kind where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Direction where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Polarity where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Constructor where
  arbitrary = pure Constructor <*> arbitrary <*> arbitrary

instance Arbitrary Argument where
  arbitrary = pure Argument <*> arbitrary <*> arbitrary

instance Arbitrary Name where
  arbitrary = do
    x1 <- choose ('A', 'M')
    x2 <- elements "aeiou"
    x3 <- choose ('a', 'z')
    x4 <- choose ('a', 'z')
    return $ Name [x1, x2, x3, x4]

instance Arbitrary Param where
  arbitrary = do
    x1 <- choose ('X', 'Z')
    x2 <- choose ('A', 'Z')
    return $ Param [x1, x2]

instance Arbitrary Protocol where
  arbitrary = do
    nrOfCtors <- choose (1, maxConstructors)
    pure Protocol <*> 
    

instance Arbitrary Type where
  arbitrary = pure TyUnit


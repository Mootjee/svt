module Generators where

import Test.QuickCheck
import System.Random
import SetOrd
import Data.List

-- QuickCheck arbitrary definition for Set
-- It first creates a list and then pass the list in the type constructor of Set
-- We also make shure that the set is sorted and all duplicates are removed

-- Generate samples by: $ generate arbitrary :: IO (Set Int) or
-- $ sample (arbitrary :: Gen (Set Int))
instance (Arbitrary a, Ord a, Eq a) => Arbitrary (Set a) where
  arbitrary = do
    list <- arbitrary
    return (Set (nub(sort list)))

-- Custom generator for Set
-- We also make shure that the set is sorted and all duplicates are removed

-- Generate samples by: $ setGenerator (1, 10) (1, 10)
setGenerator :: (Int, Int) -> (Int, Int) -> IO (Set Int)
setGenerator lengthRange elementRange = do
  listLength <- randomRIO lengthRange -- Get a radom list length in the given range
  randomList <- listGenerator listLength elementRange -- Create the list
  return (Set (nub (sort randomList))) -- Create a Set of the list

-- Custom generator for list using IO
-- We also make shure that the set is sorted and all duplicates are removed

-- Generate samples by: $ listGenerator 10 (1, 10)
listGenerator :: Int -> (Int, Int) -> IO [Int]
listGenerator 0 _ = return [] -- Edge case
listGenerator listLength elementRange = do
  randomElement <- randomRIO elementRange -- Get a random element in the given range
  randomList <- listGenerator (listLength - 1) elementRange -- recursion
  return (randomElement : randomList) -- Create a sorted list

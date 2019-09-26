module Generators where

import Test.QuickCheck
import System.Random
import SetOrd
import Data.List


instance (Arbitrary a, Ord a, Eq a) => Arbitrary (Set a) where
  arbitrary = do
    list <- arbitrary
    return (Set (sort list))


customSetGenerator :: (Int, Int) -> (Int, Int) -> IO (Set Int)
customSetGenerator lengthRange elementRange = do
  listLength <- randomRIO lengthRange
  randomList <- listGenerator listLength elementRange
  return (Set randomList)


listGenerator :: Int -> (Int, Int) -> IO [Int]
listGenerator 0 _ = return []
listGenerator listLength elementRange = do
  randomElement <- randomRIO elementRange
  randomList <- listGenerator (listLength - 1) elementRange
  return (sort(randomElement : randomList))

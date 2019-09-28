module Lecture4

where

import Data.List
import Data.Char
import Test.QuickCheck
import System.Random
import SetOrd

-----------------------------------------------------
-- LAB 4
-----------------------------------------------------
-- Prerequisites:
-- Read or reread Chapter 4 and 5 of The Haskell Road.
-- Make a list of questions on specific points that cause difficulty of understanding.
-- questions:


-- Exercise 1:
-- Implement a random data generator for the datatype Set Int, where Set is as defined in SetOrd.hs.
-- First do this from scratch, next give a version that uses QuickCheck to random test this datatype.
-- (Deliverables: two random test generators, indication of time spent.)

-- QuickCheck arbitrary definition for Set
-- It first creates a list and then pass the list in the type constructor of Set
-- We also make sure that the set is sorted and all duplicates are removed

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
---------------------------------------------------------------------------
-- 2) Implement operations for set intersection, set union and set difference, for the datatype Set defined in SetOrd.hs. Next, use automated testing to check that your implementation is correct. First use your own generator, next use QuickCheck.
-- (Deliverables: implementations, test properties, short test report, indication of time spent.)
---------------------------------------------------------------------------
-- 3) Suppose we implement binary relations as list of pairs, Haskell type [(a,a)]. Assume the following definition:
-- > type Rel a = [(a,a)]
-- Use this to implement a function
-- symClos :: Ord a => Rel a -> Rel a
-- that gives the symmetric closure of a relation, where the relation is represented as an ordered list of pairs. E.g., symClos [(1,2),(2,3),(3,4)] should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)].
-- (Deliverables: Haskell program, indication of time spent.)

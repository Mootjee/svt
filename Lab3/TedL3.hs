module Lecture3

where

import Data.List
import Data.Char
import Test.QuickCheck
import System.Random
import SetOrd

-----------------------------------------------------
-- LAB 3
-----------------------------------------------------
-- excercise 1
contradiction :: Form -> Bool

tautology :: Form -> Bool

-- | logical entailment
entails :: Form -> Form -> Bool

-- | logical equivalence
equiv :: Form -> Form -> Bool


-- excersice 4
module Generators where
import Test.QuickCheck
import Lecture3
import Control.Monad

-- This code was created with the example of the paper: quickcheck: a lightweight tool for random testing of haskell programs
-- The code was based on Tree construction example

-- Define Arbitrary Form, where we pass size bound so we don't create overly
-- big formulas
instance Arbitrary Form where
  arbitrary = sized arbForm -- <= Size bound

-- Case if size 0
arbForm 0 = liftM Prop (choose (1, 10))

-- Case if size != 0
arbForm n = frequency [
  (1, liftM2 Impl arbitrary arbitrary),  --pass two forms
  (1, liftM2 Equiv arbitrary arbitrary), -- pass two forms
  (1, liftM Neg arbitrary),  -- pass a form
  (1, liftM Cnj (vectorOf 2 arbitrary)), -- pass a list of two forms
  (1, liftM Dsj (vectorOf 2 arbitrary)), -- pass a list of two forms
  (1, liftM Prop (choose (1, 10))) -- pass a prop, give it a random name
  ] where arbitrary = arbForm (n `div` 2) -- define recursion formula used for the subformulas

-- excersice 6
type Clause  = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
cnf2cls (Prop x) = [[x]]
cnf2cls (Neg (Prop x)) = [[negate x]]
cnf2cls (Cnj x) = makeListFromProps x
cnf2cls (Dsj x) = makeListFromProps x

makeListFromProps x = concat [cnf2cls a | a <- x]

-- The number of Prop's should be the same in the form and in the cls

countPropsCls :: Clauses -> Int
countPropsCls c | null c = 0
                | otherwise = length (concat c)

sumOfProps x = sum [countPropsCls a | a <- x]

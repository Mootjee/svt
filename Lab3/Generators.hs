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

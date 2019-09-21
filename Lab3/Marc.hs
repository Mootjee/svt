module Marc
where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Test.QuickCheck.Monadic
import Control.Monad
import Debug.Trace
import Generators

-- Exercise 1
-- | logical contradiction
contradiction :: Form -> Bool
contradiction form = not (and [evl x form | x <- allVals form])

-- Property for quickCheck where we apply a conditional law to filter out
-- contradictions
prop_Contradiction :: Form -> Property
prop_Contradiction form =
    not (and [evl x form | x <- allVals form]) ==>
      contradiction form

-- | logical tautology
tautology :: Form -> Bool
tautology form = and [evl x form | x <- allVals form]

-- Property for quickCheck where we apply a conditional law to filter out
-- tautologies
prop_Tautology :: Form -> Property
prop_Tautology form =
  and [evl x form | x <- allVals form] ==>
    tautology form

-- | logical entailment
entails :: Form -> Form -> Bool
entails formA formB = all (\values -> if evl values formA then evl values formB else True) (genVals(propNames form1 ++ propNames form2))

-- | logical equivalence
equiv  :: Form -> Form -> Bool
equiv formA formB = all (\values -> evl values formA == evl values formB) (genVals(propNames form1 ++ propNames form2))

exerciseOne = do
  print "Excersice One"
  print "Contradiction"
  putStrLn (show (Cnj [Prop 1, Neg (Prop 1)]))
  print (contradiction (Cnj [Prop 1, Neg (Prop 1)]))
  quickCheck prop_Contradiction
  print "Tautology"
  putStrLn (show (Dsj [Prop 1, Neg (Prop 1)]))
  print (tautology (Dsj [Prop 1, Neg (Prop 1)]))
  quickCheck prop_Tautology
  print "Entails"
  putStrLn (show (Cnj [Prop 1,Prop 2]))
  putStrLn (show (Dsj [Prop 1, Prop 2]))
  print (entails (Cnj [Prop 1,Prop 2]) (Dsj [Prop 1, Prop 2]))
  print "equiv"

-- Time spent ~3 hour

-- Exercise 2

-- A formula is equal if the formula has the same elements and the same thruth tables.
-- Thruth is here an extra check, because the property names could be diffent and we want to test that.

-- QuickCheck Property where the original formula must be equal to the parsed formula and satisfiable
prop_Parsing :: Form -> Property
prop_Parsing form =
  satisfiable form ==>  -- We check if atleast the formula is satisfiable, so we know that contradiction formulas will not be tested
    (head(parse(show form)) == form) && ([evl value form | value <- allVals form ] == [evl value (head(parse (show form)))| value <- allVals (head(parse (show form)))])

exerciseTwo = do
  print "Excersice Two"
  print "Formula to parse"
  print (show (Cnj [Prop 1,Prop 2]))
  print "Parsed formula must be equeal to original formula and have the same truth table"
  print ((parse "*(1 2)") == ([Cnj [Prop 1, Prop 2]]) &&  [evl value (Cnj [Prop 1, Prop 2]) | value <- allVals (Cnj [Prop 1,Prop 2])] == [evl value (head(parse "*(1 2)")) | value <- allVals (head(parse "*(1 2)"))])
  print "QuickCheck implementation"
  quickCheck prop_Parsing

-- Time spent ~1 hour

-- Excersice 3
-- The  task is to write a Haskell program for converting formulas into CNF.

-- This function is based on the nnf function in the Lecture3.hs
-- We will map all the forms against a lookup table
convertToCNF :: Form -> Form
convertToCNF form | (Impl subformA subformB ) == form = form
                  | otherwise = form

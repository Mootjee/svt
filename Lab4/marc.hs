import Generators
import SetOrd
import Data.Tuple
import Data.List
import Lecture4
import Test.QuickCheck


-- =============================================================================
-- EXERSICE 2
-- =============================================================================
-- RESULTS
-- =============================================================================
-- IMPLEMENTATION

setIntersection :: (Ord a) => Set a -> Set a -> Set a
setIntersection (Set xs) setY = Set [x |x <- xs, inSet x setY]
-- =============================================================================


-- =============================================================================
-- EXERSICE 3
-- =============================================================================
-- RESULTS

-- *Main> exerciseThree
-- "Applying Symmetric closure on list"
-- [(1,2),(2,3),(3,4)]
-- "Result:"
-- [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
-- =============================================================================
-- IMPLEMENTATION


-- Symmetric closure => symmetry:  ∀x(xRy ⇒ yRx)

-- if for all x, y ∈ A: if xRy then yRx.
type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
-- Swap tuple and check if in list else add to new list
-- and concat with original list
symClos xs = sort (xs ++ sort[swap x | x <- xs, notElem (swap x) xs])

exerciseThree = do
  print "exercise Three "
  print "Applying Symmetric closure on list"
  print [(1,2),(2,3),(3,4)]
  print "Result:"
  print (symClos [(1,2),(2,3),(3,4)])
-- =============================================================================

-- =============================================================================
-- EXERSICE 4
-- =============================================================================
-- RESULTS
-- =============================================================================
-- IMPLEMENTATION
-- serial => Linear: ∀x(x ∈ A)∃(y ∈ A) -> xRy

-- if for all x ∈ A: if x ∈ A ∃ y ∈ A then xRy

-- Relations A = [(1,2), (2,3), (3,1)] in domain D = [1,2,3,4]
-- A is serial

-- Relations B = [(1,2), (2,3)] in domain D = [1,2,3,4]
-- B is not serial

-- Relations C = [(1,2), (2,3), (3,1)] in domain D = [3,4,5,6]
-- C is not serial
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial xs ys = not (null ys || null xs)
  && ((xs, ys, 0) $$
    fix (\ f (xs, ys, n) -> ((length ys == n) || (fstTupleElem (snd (ys !! n)) ys && elem (snd (ys !! n)) xs) && f (xs, ys, n + 1))))

-- Check is given element is a first element in a list of tuples
-- 1 [(1,2), (2, 3)] => True
-- 3 [(1,2), (2, 3)] => False
fstTupleElem :: Eq a => a -> Rel a -> Bool
fstTupleElem _ [] = False
fstTupleElem n (x:xs) = (n == fst x) || fstTupleElem n xs

prop_isSerialCheck :: [Int] -> Rel Int -> Property
prop_isSerialCheck xs ys = length xs > 2 && length ys > 2
  && all (\tuple -> elem (fst tuple) xs && elem (snd tuple) xs) ys ==>
    isSerial xs ys


-- R = {(x, y) | x = y(mod n)}  n > 0
-- We take a simple example:
-- 1 = 3 mod 2 => (1, 3)
-- 3 = 7 mod 4 => (3, 7)
-- 7 = 15 mod 8 => (7, 15)
-- The relations will only grow bigger, but the problem that arises is that
-- there must be an x ∈ R where (x, 1).
-- This can't be the case because you need to have y bigger than x in
-- order to make a valid case of x = y (mode n)

exerciseFour = do
  print "relation A"
  print [(1,2), (2,3), (3,1)]
  print "in domain"
  print [1,2,3]
  print "isSerial?"
  print (isSerial [1,2,3] [(1,2), (2,3), (3,1)])
  print "relation B"
  print [(1,2), (2,3)]
  print "in domain"
  print [1,2,3]
  print "isSerial?"
  print (isSerial [1,2,3] [(1,2), (2,3)])
  print "relation C"
  print [(1,2), (2,3), (3,1)]
  print "in domain"
  print [3,4,5,6]
  print "isSerial?"
  print (isSerial [3,4,5,6] [(1,2), (2,3), (3,1)])
  print "quickCheck test"
  quickCheck prop_isSerialCheck
-- =============================================================================


-- =============================================================================
-- EXERSICE 5
-- =============================================================================
-- RESULTS
-- =============================================================================
-- IMPLEMENTATION
-- Use the datatype for relations from the previous exercise, plus

--  E.g., trClos [(1,2),(2,3),(3,4)] should give [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

--  (x,y) ∈ A && (y,z) ∈ A -> (x,z) ∈ A
trClos :: Ord a => Rel a -> Rel a
-- First check for the pairs if there is a relation (x,y) ∈ A && (y,z) ∈ A where y == y
-- If true then create pair (x, z)
-- Remove all duplicates
-- Do this in recursion, because new pairs can be formed that form the relation (x,y) ∈ A && (y,z) ∈ A where y == y
-- Run recursion till the result can no longer be sorted
trClos = fp (\xs -> sort (nub (xs ++ xs @@ xs)))

exerciseFive = do
    putStrLn "Example from exercise 6"
    let exampleOne = [(1,2),(2,3),(3,4)]
    let resultOne = [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
    putStrLn $ "input " ++ show exampleOne
    putStrLn $ "output " ++ show resultOne
    putStrLn $ "Result: " ++ show (trClos exampleOne == resultOne)
    let exampleTwo = [(1,1), (2,2), (3,3), (4,4)]
    let resultTwo = [(1,1), (2,2), (3,3), (4,4)]
    putStrLn $ "input " ++ show exampleTwo
    putStrLn $ "output " ++ show resultTwo
    putStrLn $ "Result: " ++ show (trClos exampleTwo == resultTwo)
    let exampleThree = [(1,2), (2,1), (2,3), (3,2)]
    let resultThree = [(1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3)]
    putStrLn $ "input " ++ show exampleThree
    putStrLn $ "output " ++ show resultThree
    putStrLn $ "Result: " ++ show (trClos exampleThree == resultThree)
-- =============================================================================


-- =============================================================================
-- EXERSICE 6
-- =============================================================================
-- RESULTS
-- =============================================================================
-- IMPLEMENTATION
-- Test the functions symClos and trClos from the previous exercises.
-- Devise your own test method for this. Try to use random test generation.
-- Define reasonable properties to test. Can you use QuickCheck? How?

-- For every (x,y) ∈ A -> (y,x) ∈ A
checkSymmetry :: Ord a => Rel a -> Bool
-- Property: Get every pair for symmetry list and check if the swap of elements is in symmetry list
-- (x,y) => swap => (y,x)
checkSymmetry xs = and [elem (swap x) symmetry  | x <- symmetry]
  where symmetry = symClos xs

checkTransitivity :: Ord a => Rel a -> Bool
-- Property: Get every pair for transitive list and check if there is a pair (a,b) and a pair (c,d)
-- and b == d then there must be a pair (a, d) in the transitive list in order to let it be transitive
checkTransitivity xs = and [elem (a,d) transitive | (a,b) <- transitive, (c,d) <- transitive, b == c]
  where
    transitive = trClos xs

exerciseSix = do
    print "QuickCheck for symClos function"
    quickCheck (checkSymmetry :: Rel Int -> Bool)
    print "QuickCheck for trClos function"
    quickCheck (checkTransitivity :: Rel Int -> Bool)

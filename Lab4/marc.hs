import Generators
import SetOrd
import Data.Tuple
import Data.List
import Lecture4
-- import Control.Monad.Fix



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

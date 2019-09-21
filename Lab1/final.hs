import           Lab1
import           Test.QuickCheck
import           Data.List

-- Exercise 01
-- We need to prove that the formula: (n* ((n + 1) * (2 * n + 1))) / 6
-- is equal to the sequence: 1^2 + 2^2 + ... + n^2
-- where n is in the set of Natural Numbers.

-- Natural number generator
genNaturalNumbers :: Gen Int
genNaturalNumbers = abs <$> (arbitrary :: Gen Int) `suchThat` (> 0)

-- left side of equation
firstFuncExOne :: Int -> Int
firstFuncExOne n = sum [ k ^ 2 | k <- [1..n]]

-- right side of equation
secondFuncExOne :: Int -> Int
secondFuncExOne n = div (n *((n + 1) * (2 * n + 1))) 6

-- Test to see if the two formulas are equal.
testExcerciseOne :: Int -> Bool
testExcerciseOne n = firstFuncExOne n == secondFuncExOne n


-- We need to prove that the formula: ((n * (n + 1)) / 2 )^2
-- is equal to the sequence: 1^3 + 2^3 + ... + n^3
-- where n is in the set of Natural Numbers.

-- Left side of equation
firstFuncTwo :: Int -> Int
firstFuncTwo n = sum [ k ^ 3 | k <- [1..n]]

-- Right side of equation
secondFuncTwo :: Int -> Int
secondFuncTwo n = div (n * (n + 1)) 2 ^ 2

-- The same generator can be applied to this problem as the problem above.

-- test to see if the formulas are equal
-- Test to see if the two formulas are equal.
testExcerciseOneTwo :: Int -> Bool
testExcerciseOneTwo n = firstFuncTwo n == secondFuncTwo n

<<<<<<< Updated upstream

-- Time spent: ~2 Hours

-- Exercise 2
--  Prove that if A is a finite list with length = n, then |P(A)| = 2^n.
=======
-- Excersice 2
-- We need to prove that the formula:  | A | = n then |P (A)| = 2 ^ n
>>>>>>> Stashed changes

-- We first make a formula that calculates the length of the subsequence
-- list. This is equal tot the amount of subsequences existing for n.
-- example: n = 3 : [ [] [1], [2], [3], [1,2], [1,3], [2,3], [1,2,3]]
-- The length of this list would be 8
firstFuncExTwo :: [Int] -> Int
firstFuncExTwo xs = length (subsequences xs)

-- Then we test the right hand side of the equation. It calculates
-- 2 ^ (the length of the input list). It returns an integer, which
-- should be equal to the amount of subsets you can make from a list.
secondFuncExTwo :: [Int] -> Int
secondFuncExTwo xs = 2 ^ length xs


-- To test if the two formula's are equal, we need to give it a lot of lists.
-- To generate the lists, we use the natural number generator we've created above.
testExcerciseTwo :: Int -> Bool
testExcerciseTwo 0 = firstFuncExTwo [] == secondFuncExTwo []
testExcerciseTwo n = n < 10 --> firstFuncExTwo [1..n] == secondFuncExTwo [1..n]
-- Also we are only testing whether firstFuncExTwo satisfies a part of its specification
-- This can be seen that we not allow lists bigger then 10
-- The reason for this, is because the functions took to much time to finish.

-- Time spent: ~2 Hours

-- Excersice 3

-- Our first step is to model the powerset function in Haskell.
-- Function explanation
-- Type declarations: we expect to get as input a list, inturn we always return
-- a list containing atleast one list (the empty list).
-- clauses: the first clause is a guard for the empty set, we will return
-- a list with a empty list as a result
-- the second clause will take the current item of the list and will
-- recursiverly compute the subsets of the remainder xs combined with element x
-- and this will be added to the subsets of the remainder xs
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

-- This is the text function by the formula 2 ^ n
powersetTester :: Int -> Int
powersetTester n = 2 ^ n

-- The test we configure is a bit altered. This is because both functions
-- expect different inputs and have different outputs.
-- Therefore we altered the test function. We make sure that the inputs
-- and outputs are correct for the two functions.
<<<<<<< Updated upstream
-- Also we are only testing whether powerset satisfies a part of its specification
-- This can be seen that we not allow lists bigger then 10
-- The reason for this, is because the functions took to much time to finish.
testExcerciseThree :: [String] -> Bool
testExcerciseThree xs = length xs < 10 --> powersetTester (length xs) == length (powerset xs)

-- Time spent: ~2 Hours
=======
-- Also to make sure the test doesn't pass huge lists to the function, the lists
-- are checked if the do not surpass a length bigger than 10
testExTwo :: [String] -> Bool
testExTwo xs = length xs <= 10 --> firstFuncExTwo (length xs) == length (secondFuncExTwo xs)
>>>>>>> Stashed changes

-- Excersice 4
-- We expect to get a list of integers, this list of intergers we are going
-- to filter on prime numbers that are reversal. Then we return this
-- resulting list.
-- Function explanation: the input will be an integer list
-- the result will be an integer list
-- When given a empty list, it will return a empty list
-- When the element x of the list xs is prime and (reversal and prime) we add it
-- to the resulting list otherwise we remove it from the list.
primeFilterEx4 :: [Integer] -> [Integer]
primeFilterEx4 [] = []
primeFilterEx4 (x:xs)  | prime x && prime (reversal x)  = x : primeFilterEx4 xs
                       | otherwise                      = primeFilterEx4 xs

<<<<<<< Updated upstream
-- I would test this function by making a predifined list consisting with numbers that have
-- the charistics of being prime and in reverse being prime. Then I would put
-- noise in this list and look if the resulting list by this function is the
-- same as the correct list.

-- Time spent: ~2 Hours

-- Excersice 5
-- First we know that the function will be passed the list [13,17,19,23,29] that
-- summerizes in 101.
-- We will add the primes one by one and check if we found the smallest number
-- First we will create the powerset and then we will calculate the minimum.
listItems = [13, 17, 19, 23, 29]

minPrimeSum :: [[Integer]] -> Integer
minPrimeSum [[]]    = 100000
minPrimeSum [x]     = addPrimes x
minPrimeSum (x:xs)  = min (addPrimes x) (minPrimeSum xs)

addPrimes :: [Integer] -> Integer
addPrimes [] = 0
addPrimes (x:xs) = x + addPrimes xs

-- This function is not correct, we tried but couldn't find the solution
-- Time spent: ~2 Hours

-- Excersice 6
-- We will use  Haskell to refute a conjecture.
--  "If p1,...,pn e.g [2,3] is a list of consecutive primes starting from 2, then (p1×⋯×pn)+1 e.g 2 * 3 + 1 = 7 is also prime."
-- We need to find the smallest counterexample?
-- Exercise 6

-- This function will calculate the product of a list of primes
-- ================================
-- primesProduct :: [Integer] -> Integer
-- primesProduct [a] = a
-- primesProduct (h:t) = h * primesProduct t
-- ================================

-- Now we need to find the counter example
-- We then want to find all the sequences where it is not a prime.
-- We do this with take
-- ================================
-- findNonPrimes :: Integer -> [Integer] -> ([Integer],Integer)
-- findNonPrimes n (x:xs) | prime ((primesProduct (take n (x:xs))) + 1) = findNonPrimes n xs
--                        | otherwise = ((take n (x:xs)), primesProduct (take n (x:xs)))
-- ================================
-- The function didn't work, we tried but couldn't find the solution
-- Time spent: ~2 Hours


=======
-- I would check by making a predifined list consisting with numbers that have
-- the charistics of being prime and in reverse being prime. Then I would put
-- noise in this list and look if the resulting list by the function is the
-- same as the ideal list.
>>>>>>> Stashed changes

main :: IO ()
main = do
  putStrLn "== Proof induction (Fail) =="
<<<<<<< Updated upstream
  quickCheckResult $ forAll genNaturalNumbers testExcerciseOne
  quickCheckResult $ forAll genNaturalNumbers testExcerciseOneTwo
  quickCheckResult $ forAll genNaturalNumbers testExcerciseTwo
  quickCheckResult testExcerciseThree
=======
  quickCheckResult testExTwo
  list <- primeFilterEx4 [1..1000]
  putStrLn list
  print $ splitOn "|" list
>>>>>>> Stashed changes
  putStrLn "\n== Proof induction (Success) =="
  putStrLn "\nDone :D"

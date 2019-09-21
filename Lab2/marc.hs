import Lab2
import Lecture2
import Test.QuickCheck

--Excersice 1
-- Helper function the amount of items in a certain index
getQuartile :: [Float] -> Float -> Float -> Int
getQuartile xs low high = length (filter(\x -> x >= low && x <= high) xs)

-- Function to count the amount of items in quartiles of distributions
distribution :: Int -> IO [Int]
distribution x = probs x >>= \xs -> return [
  getQuartile xs 0.0 0.25,
  getQuartile xs 0.25 0.50,
  getQuartile xs 0.50 0.75,
  getQuartile xs 0.75 1.00]

-- Helper function to calculate the percentage
getPercentage :: Int -> Int -> Float
getPercentage total part = (fromIntegral part :: Float) / (fromIntegral total :: Float) * 100.00

-- Function that calculates all percentage distributions
percentageDistribution :: Int -> IO [Float]
percentageDistribution n = distribution n >>= \xs -> return (map (getPercentage n) xs)

-- Post condition for percentageDistribution
percentageDistributionPostCondition :: [Float] -> Bool
percentageDistributionPostCondition xs = all (\x -> x >= 20.0 && x <= 30.0) xs

-- Helper functions for the test
checkTestResult :: Bool -> String
checkTestResult True  = "Test succeeded"
checkTestResult False  = "Test failed"

-- Custom test
testpercentageDistribution :: Int -> Int -> IO ()
testpercentageDistribution k n =
  if k == n then print (show n ++ " tests passed")
    else do
      print . checkTestResult . percentageDistributionPostCondition =<< percentageDistribution 1000
      testpercentageDistribution (k + 1) n

--Time spent ~ 3 hours

-- Excersice 3
-- First we model all the formulas in Haskell
formulaOne :: Int -> Bool
formulaOne x = even x && x > 3

formulaTwo :: Int -> Bool
formulaTwo x = even x || x > 3

formulaThree :: Int -> Bool
formulaThree x = (even x && x > 3) || even x

-- Then we put all the formulas in a list, uniquely indentifying them with a string
formulas = [("formula one", formulaOne),  ("formula two", formulaTwo), ("formula three", formulaThree)]

-- We then apply quicksrt on the list of formulas, where we compare with the
-- stronger and weakers functions given in Lecture2.hs
sortedFormulas :: [Int] -> [(String, (Int -> Bool))] -> [(String, (Int -> Bool))]
sortedFormulas _ [] = []
sortedFormulas range (p:xs) = (sortedFormulas range lesser) ++ [p] ++ (sortedFormulas range greater)
    where
      lesser = filter (\(_,x) -> weaker range (snd p) x) xs
      greater = filter (\(_,x) -> stronger range (snd p) x) xs

display :: [(String, (Int -> Bool))] -> IO()
display zs = sequence_ [putStrLn a | (a,b) <- zs]

--Time spent ~ 3 hours

-- Excersice 4
-- First we create a permutation function
permutations :: [Int] -> [[Int]]
permutations [] = return []
permutations (x:xs) = permutations xs >>= \ xs -> ins x xs
    where
    ins :: Int -> [Int] -> [[Int]]
    ins x []     = [[x]]
    ins x (y:ys) = [x:y:ys] ++  map (y:) (ins x ys)

-- We define a function isSameLength because a permutation is a finite list
-- with the same length as the original list
isSameLength :: [a] -> [a] -> Bool
isSameLength [] [] = True
isSameLength listOne listTwo = length listOne == length listTwo

-- This function will count the amount of occurences in the give list for
-- a specific item and compare it to the amount of occurences of the same item in the other list
hasSameAmountOfOccurences :: Eq a => [a] -> [a] -> Bool
hasSameAmountOfOccurences [] [] = True
hasSameAmountOfOccurences listOne listTwo = all (\x -> count x listTwo == count x listOne) listOne

count :: Eq a => a -> [a] -> Int
count x xs =  length (filter (==x) xs)

-- We define the is permutation, in the function definition we make use of the
-- Eq constraint, we use this because the paremeters must be compared for
-- equality and therefore need to be tested on equality.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation listOne listTwo | listOne == listTwo = False
                              | hasSameAmountOfOccurences listOne listTwo && isSameLength listOne listTwo = True
                              | otherwise = False

-- I could not figure out the generators

-- generatePermutations :: Gen ([Int], [Int])
-- generatePermutations =  do
--     k <- choose (1, 100)
--     list <- sequence [ arbitrary | _ <- [1..k] ]
--     return (list, (permutations list)!!1)
--
--
-- flexList :: Arbitrary a => Gen [a]
-- flexList = sized $ \n ->
--   frequency
--     [(n, (:) <$> arbitrary <*> flexList)]
--
-- arbitraryList :: Arbitrary a => Gen [a]
-- arbitraryList =
--   sized $
--     \n -> do
--       k <- choose (1, n)
--       sequence [ arbitrary | _ <- [1..k] ]
--
--Time spent ~ 3 hours

main :: IO ()
main = do
  putStrLn "== Start excersices =="
  putStrLn "exercise 1"
  putStrLn "start test"
  testpercentageDistribution 10 100
  putStrLn "End of test exercise 1"
  putStrLn "Excersice 3"
  display (sortedFormulas [1..10] formulas)
  putStrLn "Test excersice 4"
  print "First list"
  print [2,2,3]
  print "Correct permutation"
  print [2,3,2]
  print "Function outcome"
  print (isPermutation [2,2,3] [2,3,2])
  print "False permutation"
  print [3,3,2]
  print "Function outcome"
  print (isPermutation [2,2,3] [3,3,2])
  putStrLn "== End excersices =="

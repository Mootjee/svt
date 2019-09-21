import Test.QuickCheck
import Data.Char
import Data.List
import Data.Function

-- Exercise 02

data Shape = NoTriangle | Equilateral
            | Isosceles  | Rectangular | Other deriving (Eq,Show)

-- This function checks whether the sides given as input represent
-- an Equilateral, Rectangular, Isosceles or an other trianlge.
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
  | checkEqu a b c = Equilateral
  | checkRec a b c = Rectangular
  | checkIso a b c = Isosceles
  | checkOther a b c = Other
  | otherwise = NoTriangle

-- This parses the tuple so that the triangle function can read the input
readTuple :: (Integer, Integer, Integer) -> Shape
readTuple (a, b, c) = triangle a b c

-- this function makes a list of tulples containing three integers
makeList :: [(Integer, Integer, Integer)]
makeList = [(a,b,c) | a <- [1..15], b <- [1..15], c <- [1..15] ]

-- This function generates tuples, where a b and c are equal.
-- since all sides are equal, all angles are equal based on the Base Angles Theorem.
-- These two properties ensure us that the generated tuples represent equal triangles.
makeEquList :: [(Integer, Integer, Integer)]
makeEquList =  [(a,b,c) | a <- [1..15], b <- [1..15], c <- [1..15], checkEqu a b c]

-- Using the pythagorian algorithm we can prove that an triangle is Rectangular.
-- The pythagorian algorithm is used in the implementation of the function that generates
-- the tuples, which ensures that the tuples generated represent a Rectangular triangle.
makeRecList :: [(Integer, Integer, Integer)]
makeRecList = [(a,b,c) | a <- [1..15], b <- [1..15], c <- [1..15], checkRec a b c]

-- According to The Base Angles Theorem If two sides of a triangle are congruent,
-- then the angles opposite those sides are congruent. Since this function generates
-- tuples that have exactly two sides that are equal, we know that we also have
-- two angles that are equal. The property of having exactly two equal angles
-- proofs that the generate tuple represents an Isosceles Traingle.
makeIsoList :: [(Integer, Integer, Integer)]
makeIsoList = [(a,b,c) | a <- [1..15], b <- [1..15], c <- [1..15], checkIso a b c]

-- This should generate triangles that are non of the above categories.
-- We can check if the generated tuple is a triangle using Triangle Inequality Theorem.
-- This theorem states that a+b >= c or a +c >= b or b + c >= a.
-- This theorem is integrated in the implementation, hence we can be certain that it
-- generates triangles, according to thte Triangle Inequality Theorem.
makeOtherList :: [(Integer, Integer, Integer)]
makeOtherList = [(a,b,c) | a <- [1..15], b <- [1..15], c <- [1..15], checkOther a b c]

-- This function generates traingles where either a+b < c, a+c < b or b + c < a.
-- Tuple with sides where this property holds are no triangle!
makeNon :: [(Integer, Integer, Integer)]
makeNon = [(a,b,c) | a <- [1..15], b <- [1..15], c <- [1..15], checkNon a b c]

-- this checks if a tuple is a Equilateral triangle
checkEqu :: Integer -> Integer -> Integer -> Bool
checkEqu a b c = (a == b && a == c)

-- this checks if a tuple is a Rectangular triangle
checkRec :: Integer -> Integer -> Integer -> Bool
checkRec a b c = (b^2) + (c^2) == a^2 || (a^2) + (c^2) == b^2 || (a^2) + (b^2) == c^2

-- this checks if a tuple is a Isosceles triangle
checkIso :: Integer -> Integer -> Integer -> Bool
checkIso a b c = (not (checkEqu a b c) && (a == b || a == c || b == c ))

-- this checks if a tuple is an other triangle
checkOther :: Integer -> Integer -> Integer -> Bool
checkOther a b c = not (checkEqu a b c) && not (checkRec a b c) && not (checkIso a b c) && (a + b >= c || a + c >= b || c + b >= a)

-- This checks if a tuple is not a Triangle
checkNon :: Integer -> Integer -> Integer -> Bool
checkNon a b c = not (a + b >= c || a + c >= b || c + b >= a)

-- This tests if all tuples in the makeEquList are Equilateral
testEqu :: Bool
testEqu = all (\x -> readTuple x == Equilateral) makeEquList

-- This tests if all tuples in the makeRecList are Rectangular
testRec :: Bool
testRec = all (\x -> readTuple x == Rectangular) makeRecList

-- This tests if all tuples in the makeIsoList are Isosceles
testIso :: Bool
testIso = all (\x -> readTuple x == Isosceles) makeIsoList

-- This tests if all tuples in the makeOtherList are Other Triangles
testOther :: Bool
testOther = all (\x -> readTuple x == Other) makeOtherList

-- This tests if all tuples in the makeNon list are No Triangles
testNon :: Bool
testNon = all (\x -> readTuple x == NoTriangle) makeNon


-- Time: 7 Hours

-- Exercise 05

-- this function takes in two lists and looks whether they are
-- derangements from each other.
isDerangements :: [Int] -> [Int] -> Bool
isDerangements [] [] = True
isDerangements [] _ = False
isDerangements _ [] = False
isDerangements (x:xs) (y:ys) = x /= y && isDerangements xs ys

-- this function returns all derangements from the list [0..n-1]
deran :: Int -> [ [Int] ]
deran n = filter (isDerangements [0..(n-1)]) (permutations [0..(n-1)])

-- The length of both lists should be equal (isDerangements [1] [] == False)
-- The same elements should be in both lists (isDerangements [1] [2] == False)
-- Derangements are symettric: (if isDerangements [0,1] [1,0] == True,
--                                 then isDerangements [1,0] [0,1] == True)

-- The length of both lists should be equal (isDerangements [1] [] == False)
-- [1..n] [1..n] = True
-- [1..n] [1..n-1] = False

-- This generator returns a tuple Ints that are not equal to each other.
-- From tuple (x, y) where x /= y, we can generate two lists [1..x] and [1..y]
-- where the length of both lists are always not equal.
genNumbers :: Gen (Int, Int)
genNumbers = (arbitrary :: Gen (Int, Int)) `suchThat` (\(x,y) -> x /= y && x >= 0 && y >= 0)

-- This test returns True if the isDerangements function returns false,
-- Which it should do for all our input since the lists are unequal in size.
testLengthList :: (Int, Int) -> Bool
testLengthList (x,y) = isDerangements [1..x] [1..y] == False

-- quickCheckResult $ forAll genNumbers testLengthList

-- For this test we have the precondition that the lists are of the same Length,
-- And that they are non empty. Thats why the [] _ case is True, since
-- In the recursion, it gets there when the first list is fully consumed

testSameElem'' :: Int -> Bool
testSameElem'' n = testSameElem' (deran n)

testSameElem' :: [ [Int] ] -> Bool
testSameElem' (x1:x2:xs) = testSameElem x1 x2

testSameElem :: [Int] -> [Int] -> Bool
testSameElem [] _ = True
testSameElem (x:xs) ys = (elem x ys) && (testSameElem xs ys)

-- The generator only generates numbers up to 10, because the Function
-- is to slow to handle much bigger numbers.
genN :: Gen Int
genN = (arbitrary :: Gen Int) `suchThat` (\x -> x >= 3 && x <= 10)

-- quickCheckResult $ forAll genN testSameElem''

-- Time: 3 Hours


-- Exercise 06

-- ROT13 ("rotate by 13 places", sometimes hyphenated ROT-13)
-- is a simple letter substitution cipher that replaces a letter with
-- the 13th letter after it, in the alphabet.

-- Because there are 26 letters (2×13) in the basic Latin alphabet, ROT13 is its own inverse;
-- to undo ROT13, the same algorithm is applied.

-- Basic variables, needed for our function below.
orda = ord 'a'
ordA = ord 'A'
places = 13
alphabetSize = 26

-- A replace function that takes a char, and replaces it with the char 13 places later
-- In the alphabet. To solve the problem that after n, we have to go back to a
-- we just negate 13 from the ord after m. This makes sure that n becomes a, and that
-- the x in chr(x) doesn't overflow into non-alphabetical characters.

replace :: Char -> Char
replace c
  | c >= 'a' && c < 'n' = chr (ord c + 13)
  | c >= 'n' && c <= 'z' = chr (ord c - 13)
  | c >= 'A' && c < 'N' = chr (ord c + 13)
  | c >= 'N' && c <= 'Z' = chr (ord c - 13)
  | otherwise = error "Wrong input!"

-- This loops over the entire string.
rot13 :: String -> String
rot13 xs = map replace xs

-- Random char generator used to make strings
charGen :: Gen Char
charGen = elements ['a'..'z']

-- Random string generator used for testing
stringGen :: Gen String
stringGen = listOf charGen

-- One of the properties of this function is that the (non empty) input
-- is not equal to the output
-- quickCheckResult $ forAll stringGen testEquality

testEquality :: String -> Bool
testEquality [] = True
testEquality x = rot13 x /= x

-- the next property is that rot13( rot13 (input)) is equal to the input
-- quickCheckResult $ forAll stringGen testRotRot

testRotRot :: String -> Bool
testRotRot [] = True
testRotRot x =  rot13 (rot13 x) == x

-- the length of the output should be equal to the length of the input
-- quickCheckResult $ forAll stringGen testLength

testLength :: String -> Bool
testLength x =  length (rot13 x) == length x

-- Time: 2 Hours


-- Exercise 07

-- 1. Check that the total IBAN length is correct as per the country. If not, the IBAN is invalid
-- 2. Move the four initial characters to the end of the string
-- 3. Replace each letter in the string with two digits, thereby expanding the string, where A = 10, B = 11, ..., Z = 35
-- 4. Interpret the string as a decimal integer and compute the remainder of that number on division by 97
-- If the remainder is 1, the check digit test is passed and the IBAN might be valid.

-- The IBAN consists of up to 34 alphanumeric characters, as follows:
-- country code using ISO 3166-1 alpha-2 – two letters
-- check digits – two digits, and
-- Basic Bank Account Number (BBAN) – up to 30 alphanumeric characters that are country-specific.

-- This function returns the country code
getCountry :: String -> String
getCountry xs = take 2 xs

-- This function returns the specified length for that country.
getLength :: String -> Int
getLength iban
  | getCountry iban == "AL"= 28
  | getCountry iban == "AD"=24
  | getCountry iban == "AZ"=28
  | getCountry iban == "BH"=22
  | getCountry iban == "BE"=16
  | getCountry iban == "BA"=20
  | getCountry iban == "BR"=29
  | getCountry iban == "BG"=22
  | getCountry iban == "CR"=22
  | getCountry iban == "HR"=21
  | getCountry iban == "CY"=28
  | getCountry iban == "DK"=18
  | getCountry iban == "DO"=28
  | getCountry iban == "DE"=22
  | getCountry iban == "SV"=28
  | getCountry iban == "EE"=20
  | getCountry iban == "FO"=18
  | getCountry iban == "FI"=18
  | getCountry iban == "FR"=27
  | getCountry iban == "GE"=22
  | getCountry iban == "GI"=23
  | getCountry iban == "GR"=27
  | getCountry iban == "GL"=18
  | getCountry iban == "GT"=28
  | getCountry iban == "HU"=28
  | getCountry iban == "IE"=22
  | getCountry iban == "IS"=26
  | getCountry iban == "IQ"=23
  | getCountry iban == "IL"=23
  | getCountry iban == "IT"=27
  | getCountry iban == "JO"=30
  | getCountry iban == "KZ"=20
  | getCountry iban == "XK"=20
  | getCountry iban == "KW"=30
  | getCountry iban == "LV"=21
  | getCountry iban == "LB"=28
  | getCountry iban == "LI"=21
  | getCountry iban == "LT"=20
  | getCountry iban == "LU"=20
  | getCountry iban == "VG"=24
  | getCountry iban == "MK"=19
  | getCountry iban == "MT"=31
  | getCountry iban == "MR"=27
  | getCountry iban == "MU"=30
  | getCountry iban == "MD"=24
  | getCountry iban == "MC"=27
  | getCountry iban == "ME"=22
  | getCountry iban == "NL"=18
  | getCountry iban == "NO"=15
  | getCountry iban == "AT"=20
  | getCountry iban == "PK"=24
  | getCountry iban == "PS"=29
  | getCountry iban == "PL"=28
  | getCountry iban == "PT"=25
  | getCountry iban == "QA"=29
  | getCountry iban == "RO"=24
  | getCountry iban == "LC"=32
  | getCountry iban == "SM"=27
  | getCountry iban == "ST"=25
  | getCountry iban == "SA"=24
  | getCountry iban == "RS"=22
  | getCountry iban == "SC"=31
  | getCountry iban == "SI"=19
  | getCountry iban == "SK"=24
  | getCountry iban == "ES"=24
  | getCountry iban == "TL"=23
  | getCountry iban == "CZ"=24
  | getCountry iban == "TN"=24
  | getCountry iban == "TR"=26
  | getCountry iban == "UA"=29
  | getCountry iban == "GB"=22
  | getCountry iban == "AE"=23
  | getCountry iban == "BY"=28
  | getCountry iban == "SE"=24
  | getCountry iban == "CH"=21
  | otherwise = error "Not a valid country!"

-- 1. Check that the total IBAN length is correct as per the country. If not, the IBAN is invalid
-- This function tests if the input iban has the specified length.
testLengthIban :: String -> Bool
testLengthIban x = (length x == getLength x)

-- 2. Move the four initial characters to the end of the string
-- This function moves the first four elements to the end of the list.
addToEnd :: String -> String
addToEnd (x1:x2:x3:x4:xs) = xs ++ [x1] ++ [x2] ++ [x3] ++ [x4]

-- This checks the char and replaces it with the specified digits.
-- The formula makes sure that A = 10 ... Z = 35 and that it skips all other characters
replaceChar :: Char -> String
replaceChar c |
 c >= 'A' && c <= 'Z' = show ((ord (c) - ord ('A')) + 10)
 | not (isAlphaNum c) = error "Non alpha numeric character found!"
 | otherwise = [c]

-- 3. Replace each letter in the string with two digits, thereby expanding the string, where A = 10, B = 11, ..., Z = 35
-- This function replaces each character in string with the two specified digits
replaceAll :: String -> [String]
replaceAll xs = map replaceChar xs

-- This converts a String list to a String
stringListToString :: [String] -> String
stringListToString xs = intercalate "" xs

-- 4. Interpret the string as a decimal integer and compute the remainder of that number on division by 97
-- This converts a String to an Int
stringToInt :: String -> Integer
stringToInt x = read x :: Integer

-- This calculates the remainder of the string interpreted as a decimal divided by 97
remainder :: Integer -> Int
remainder x = fromIntegral (x `mod` 97)

-- This function validates the iban number
-- It first checks whether the iban has the correct specified length.
-- Then it checks if the remainder iban interpreted as a decimal, with the first four
-- elements moved tho the end is equal to 1. If this is the case the function returns
-- True and this might be a valid Iban. If not, the Iban is unvalid!

iban :: String -> Bool
iban x
  | testLengthIban x == True = (remainder (stringToInt (stringListToString (replaceAll (addToEnd x))))) == 1
  | otherwise = False

-- Found this list of valid Ibans. This will be used to test
correctIbanList =[
  "IE29AIBK93115212345678",
  "AD1200012030200359100100",
  "AT611904300234573201",
  "BE68539007547034",
  "CZ6508000000192000145399",
  "DK5000400440116243",
  "EE382200221020145685",
  "GI75NWBK000000007099453",
  "GR1601101250000000012300695",
  "ES9121000418450200051332",
  "IT60X0542811101000000123456",
  "GB29NWBK60161331926819",
  "CY17002001280000001200527600",
  "LV80BANK0000435195001",
  "PL27114020040000300201355387",
  "LI21088100002324013AA",
  "LT121000011101001000",
  "LU280019400644750000",
  "MT84MALT011000012345MTLCAST001S",
  "NO9386011117947",
  "NL91ABNA0417164300",
  "PT50000201231234567890154",
  "FR1420041010050500013M02606",
  "SK3112000000198742637541",
  "SI56191000000123438",
  "SE3550000000054910000003",
  "CH9300762011623852957",
  "DE89370400440532013000"]

-- running the iban example list through the iban function to test validity.
testIbanCorrect :: [String] -> Bool
testIbanCorrect xs = all (==True) (map iban xs)

-- I've modified the list of ibans, by replacing random digits.
-- This should all result in non-valid ibans.
incorrectIbanList =[
  "IE29AIBK93115212345675",
  "AD1200012030200359100160",
  "AT611904300234573251",
  "BE68539007547024",
  "CZ6508000000191000145399",
  "DK5000400440114243",
  "EE382200221020645685",
  "GI75NWBK000000807099453",
  "GR1601101250007000012300695",
  "ES9121000418456200051332",
  "IT60X0542811101500000123456",
  "GB29NWBK60161336926819",
  "CY17002001280004001200527600",
  "LV80BANK0000435395001",
  "PL27114020040002300201355387",
  "LI21088100002323013AA",
  "LT121000011101005000",
  "LU280019400644750500",
  "MT84MALT011000012645MTLCAST001S",
  "NO9386011117943",
  "NL91ABNA0417164500",
  "PT50000201231234767890154",
  "FR1420041010050506013M02606",
  "SK3112000000198742537541",
  "SI56191000000123433",
  "SE3550000000054910400003",
  "CH9300762011723852957",
  "DE89370400440532013040"]

-- running the incorrect iban list through the iban function to test validity.
-- These should all return False. (non-Valid)
testIbanIncorrect :: [String] -> Bool
testIbanIncorrect xs =  all (==False) (map iban xs)

-- The list is modified to contain more or less characters than specified.
-- The length of the iban is then incorrect, which means it should return false.
-- The strings are modifed by randomly adding a number, or removing one.
incorrectLengthList =[
  "IE29AIBK931152212345678",
  "AD120001203020359100100",
  "AT6119043002334573201",
  "BE6853900754704",
  "CZ65080000001942000145399",
  "DK500040044011643",
  "EE3822002210201545685",
  "GI75NWBK00000007099453",
  "GR16011012500030000012300695",
  "ES912100041845000051332",
  "IT60X05428111031000000123456",
  "GB29NWBK6016133926819",
  "CY1700200128000001200527600",
  "LV80BANK000045195001",
  "PL2711402004000300201355387",
  "LI210881000052324013AA",
  "LT1210000111051001000",
  "LU2800194006447650000",
  "MT84MALT0110000162345MTLCAST001S",
  "NO938601111797",
  "NL91ABNA041764300",
  "PT5000020123234567890154",
  "FR142004101050500013M02606",
  "SK31120000005198742637541",
  "SI56191000000623438",
  "SE3550000000057910000003",
  "CH93007620116238852957",
  "DE893704004405320913000"]

-- The order in which the iban characters apear is of great importance.
-- This means that a shuffeled version of the iban numbers should not
-- pass the validity test. (https://onlinerandomtools.com/shuffle-letters)
-- to avoid the country code error, we left the country code in front

shuffledList =[
  "IE29AIBK931152212345687",
  "AD120001203020359100010",
  "AT6119043002334573210",
  "BE6853900754740",
  "CZ65080000001924000145399",
  "DK500040044011634",
  "EE3822002210205145685",
  "GI75NWBK00000000799453",
  "GR16011012500003000012300695",
  "ES912100041845000501332",
  "IT60X05428111031000000213456",
  "GB29NWBK6016133926891",
  "CY1700200128000001200257600",
  "LV80BANK000045195010",
  "PL2711402004000300210355387",
  "LI21088100005232401A3A",
  "LT1210000111051000100",
  "LU2800194006447605000",
  "MT84MALT0110000162435MTLCAST001S",
  "NO938601111797",
  "NL91ABNA041746300",
  "PT5000020123324567890154",
  "FR142004101005500013M02606",
  "SK31120000005918742637541",
  "SI56191000000263438",
  "SE3550000000075910000003",
  "CH93007620116328852957",
  "DE893704004405302913000"]

-- This list consist of ibans with a non-alpha numeric character.
nonAlphaNumeric = [
  "SK311200.000198742637541",
  "SI561910,0000123438",
  "SE355000-000054910000003",
  "CH930076=011623852957",
  "DE893704+0440532013000"]

-- Since the function throws an error for non-alphanumeric characters,
-- i cant run this whole list through the function. I've just
-- picked 5 random strings from this list to test.

--iban "SK311200.000198742637541"
-- *** Exception: Non alpha numeric character found!
--CallStack (from HasCallStack):
--  error, called at Ass2.hs:291:25 in main:Main

--iban "SI561910,0000123438"
-- *** Exception: Non alpha numeric character found!
--CallStack (from HasCallStack):
--  error, called at Ass2.hs:291:25 in main:Main

--iban "SE355000-000054910000003"
-- *** Exception: Non alpha numeric character found!
--CallStack (from HasCallStack):
--  error, called at Ass2.hs:291:25 in main:Main

--iban "CH930076=011623852957"
-- *** Exception: Non alpha numeric character found!
--CallStack (from HasCallStack):
--  error, called at Ass2.hs:291:25 in main:Main

--iban "DE893704+0440532013000"
-- *** Exception: Non alpha numeric character found!
--CallStack (from HasCallStack):
--  error, called at Ass2.hs:291:25 in main:Main




-- The checkdigit is country specific, which makes it very hard to test against.
-- We could test it for 1, or a small amount of countries, but this wouldn't say
-- much in how good our validator works, besides for a few countries.

-- The test process can in theory be automated by having a generator that generates
-- Valid ibans, and a generator that generates non-valid ibans.
-- It is very hard to generate valid ibans, since the check digits are country
-- specific, where some countries dont have them, and other countries have them
-- in two places in the iban. Generating non-valid ibans is very easy, you should
-- just make sure that the generated string misses atleast one property that the iban
-- should have, such as the size, the remainder being 1, the country code being valid, ect.

-- Time: 3 Hours


-- Bonus problem 1

-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.

multiples :: Int
multiples = sum ([x | x <- [1..1000], x `mod` 3 == 0 || x `mod` 5 == 0])

-- Bonus problem 2

-- Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:
-- 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
-- By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

-- generates infinite fib numbers
fib :: [Integer]
fib =  0 : 1 : zipWith (+) fib (tail fib)

-- takes the even temrs of the first 34 terms generated by fib function.
-- the last term (34) is 3524578, the one after that (35) is 5702887
-- Answer: 4613732
fib' :: Integer
fib' = sum (filter (even) (take 34 fib))

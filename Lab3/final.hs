module Marc
where

import Test.QuickCheck
import Lecture3
import Generators
import SetOrd

-- ============================================================================
-- Exercise 1
-- Result
-- "Excersice One"
-- "Contradiction"
-- *(1 -1)
-- True
-- +++ OK, passed 100 tests.
-- "Tautology"
-- +(1 -1)
-- True
-- *** Gave up! Passed only 28 tests; 1000 discarded tests.
-- "Entails"
-- *(1 2)
-- +(1 2)
-- True
-- "equiv"

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

-- Run with ghci :load finals
-- exerciseOne
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
-- ============================================================================
-- Exercise 2

-- Result
-- "Excersice Two"
-- "Formula to parse"
-- "*(1 2)"
-- "Parsed formula must be equeal to original formula and have the same truth table"
-- True
-- "QuickCheck implementation"
-- +++ OK, passed 100 tests; 2 discarded.

-- A formula is equal if the formula has the same elements and the same thruth tables.
-- Thruth is here an extra check, because the property names could be diffent and we want to test that.

-- QuickCheck Property where the original formula must be equal to the parsed formula and satisfiable
prop_Parsing :: Form -> Property
prop_Parsing form =
  satisfiable form ==>  -- We check if atleast the formula is satisfiable, so we know that contradiction formulas will not be tested
    (head(parse(show form)) == form) && ([evl value form | value <- allVals form ] == [evl value (head(parse (show form)))| value <- allVals (head(parse (show form)))])

-- Run with ghci :load finals
-- exerciseTwo
exerciseTwo = do
  print "Excersice Two"
  print "Formula to parse"
  print (show (Cnj [Prop 1,Prop 2]))
  print "Parsed formula must be equeal to original formula and have the same truth table"
  print ((parse "*(1 2)") == ([Cnj [Prop 1, Prop 2]]) &&  [evl value (Cnj [Prop 1, Prop 2]) | value <- allVals (Cnj [Prop 1,Prop 2])] == [evl value (head(parse "*(1 2)")) | value <- allVals (head(parse "*(1 2)"))])
  print "QuickCheck implementation"
  quickCheck prop_Parsing

-- Time spent ~1 hour
-- ============================================================================
-- Excersice 3

-- Result
-- "Excersice Three"
-- +++ OK, passed 100 tests; 3 discarded.

-- The  task is to write a Haskell program for converting formulas into CNF.

-- A valid conjunctional normal form conforms to the following grammar
-- L ::= p | ¬p
-- D ::= L | L ∨ D
-- C ::= D | D ∧ C

cnf :: Form -> Bool
cnf (Cnj xs) = all clause xs -- conjuction of clauses  (formA) ∧ (formB) ∧ (formC) ∧ (formD) ∧ (formE)
cnf form = clause form || literal form -- it is a clause: p ∨ a or literal p, ¬p

clause :: Form -> Bool
clause (Dsj xs) = all literal xs -- disjunction of literals  p ∨ ¬p ∨ q ∨ ¬q
clause form = literal form -- it must be a literal p, ¬p

literal :: Form -> Bool
literal (Prop _) = True -- p
literal (Neg (Prop _)) = True -- ¬p
literal _ = False -- it is not a literal

-- This function doesn't work correctly, there is a problem with convertToCNF (Dsj xs) = Dsj (map convertToCNF xs)
-- and convertToCNF (Cnj xs) = Cnj (map convertToCNF xs), we needed more time for this to fix this

convertToCNF :: Form -> Form
convertToCNF (Prop a) = Prop a -- Nothing to do
convertToCNF (Neg (Prop a)) = Neg (Prop a) -- Nothing to do
convertToCNF (Neg (Neg subform)) = convertToCNF subform -- P ≡ ¬¬P (law of double negation)
convertToCNF (Dsj xs) = Dsj (map convertToCNF xs) -- P ∨ (Q ∧ R) ≡ (P ∨ Q) ∧ (P ∨ R) (distribution laws)
convertToCNF (Cnj xs) = Cnj (map convertToCNF xs) -- P ∧ (Q ∨ R) ≡ (P ∧ Q) ∨ (P ∧ R) (distribution laws)
convertToCNF (Neg (Cnj subform)) = convertToCNF (Dsj (map (convertToCNF . Neg) subform)) -- ¬(P ∧ Q) ≡ ¬P ∨ ¬Q (DeMorgan laws).
convertToCNF (Neg (Dsj subform)) = convertToCNF (Cnj (map (convertToCNF . Neg) subform)) -- ¬(P ∨ Q) ≡ ¬P ∧ ¬Q (DeMorgan laws).
convertToCNF (Impl subformA subformB) = convertToCNF (arrowfree (Impl subformA subformB)) -- (P ⇒ Q) ≡ ¬P ∨ Q, ¬(P ⇒ Q) ≡ P ∧ ¬Q
convertToCNF (Equiv subformA subformB) = convertToCNF (arrowfree (Equiv subformA subformB))
convertToCNF form = form -- Nothing to do

-- This is a simple function (less fancy) to construct a formula to conjunctional normal form. It is created because the fancy one didn't work
-- To give an equivalent for ϕ in CNF, all you have to do is list the conjuctions of the rows in the truth
-- table where the formula turns out False. This is similiar to what we did in workshop 3 but then for CNJ.
constructCNF :: Form -> Form
constructCNF form | tautology form = Dsj [Neg (Prop 1), Prop 1]  -- easy edge case :)
                  | otherwise = Cnj cnfForm where  -- Construct a conjuction of disjunctions
                      falseOutcomes = filter (\val -> not ( evl val form)) (allVals form) -- Get all the false outcomse
                      cnfForm = map constructClause falseOutcomes -- Construct all the disjunctions

constructClause :: Valuation -> Form
constructClause xs = Dsj (map constructLiteral xs) -- Construct disjunctions of literals

constructLiteral :: (Name,Bool) -> Form
constructLiteral value = if snd value then Neg (Prop (fst value)) else Prop (fst value) -- Simple literal construction

-- QuickCheck property
prop_No_Tautology_CNF :: Form -> Property
prop_No_Tautology_CNF form =
  not (tautology form) ==> -- We don't want to check a tautology
    cnf (constructCNF form) && [evl value (constructCNF form) | value <- allVals (constructCNF form)] == [evl value form | value <- allVals form] -- Check if it is in cnf and the thruth tables are the same

-- Run with ghci :load finals
-- exerciseThree
exerciseThree = do
  print "Excersice Three"
  quickCheck prop_No_Tautology_CNF

-- Time spent ~6 hour
-- ============================================================================
-- Excersice 4

-- See Generators.hs file, We also implemented the generators in the
-- test cases of all the excersices
-- ============================================================================
-- Excersice 5

-- Result
-- The test failed, probably because our test is not correct
-- *** Failed! Falsified (after 30 tests):
-- *(+(--*(2 9) -*(7 -7)) *(+(9 +(7 (4<=>5))) 5))


sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

-- countLiterals function will count the amount of literals in a function.
-- This will help us detect if we can put the formula in the sub function.
countLiterals :: Form -> Int
countLiterals (Cnj xs) = sum(map countLiterals xs)
countLiterals (Dsj xs) = sum(map countLiterals xs)
countLiterals (Impl subformA subformB) = sum(map countLiterals [subformA, subformB])
countLiterals (Equiv subformA subformB) = sum(map countLiterals [subformA, subformB])
countLiterals (Prop name) = 1
countLiterals (Neg form) = countLiterals form

prop_sub :: Form -> Property
prop_sub form = --The form must atleast contain 1 element
    countLiterals form >= 1 ==> -- There must be atleast one literal in the formula
      checkSubSet (sub form)

checkSubSet :: Set Form -> Bool
checkSubSet (Set xs) = all satisfiable xs -- Every sub formula must be satisfiable

-- implementation of nsub function
nsub :: Form -> Int
nsub (Prop x) = 1
nsub (Neg f) = 1 + nsub f
nsub f@(Cnj [f1,f2]) =  nsub f1 + nsub f2
nsub f@(Dsj [f1,f2]) =  nsub f1 + nsub f2
nsub f@(Impl f1 f2) = nsub f1 + nsub f2
nsub f@(Equiv f1 f2) = nsub f1 + nsub f2

-- Run with ghci :load finals
-- exerciseFive
exerciseFive = do
  print "Excersice Five"
  quickCheck prop_sub

type Clause  = [Int]
type Clauses = [Clause]

-- Clauses should be read disjunctively, and clause lists conjunctively.
-- So 5 represents the atom p5, −5 represents the literal ¬p5, the clause [5,−6] represents p5∨¬p6, and the clause list [[4],[5,−6]] represents the formula p4∧(p5∨¬p6).

cnf2cls :: Form -> Clauses
cnf2cls (Prop x) = [[x]]
cnf2cls (Neg (Prop x)) = [[negate x]]
cnf2cls (Cnj x) = makeListFromProps x
cnf2cls (Dsj x) = makeListFromProps x

makeListFromProps x = concat (map cnf2cls x)

-- The number of Prop's should be the same in the form and in the cls

countPropsCls :: Clauses -> Int
countPropsCls c
 | null c = 0
 | otherwise = length (concat c)

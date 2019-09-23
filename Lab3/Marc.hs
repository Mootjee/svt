module Marc
where

import Test.QuickCheck
import Lecture3
import Generators

-- ============================================================================
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
-- ============================================================================
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
-- ============================================================================
-- Excersice 3
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

exerciseThree = do
  print "Excersice Three"
  quickCheck prop_No_Tautology_CNF

-- Time spent ~6 hour

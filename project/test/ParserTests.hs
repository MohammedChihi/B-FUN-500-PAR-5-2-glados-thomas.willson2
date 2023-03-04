module ParserTests (
    testParserFunctions
) where

-- checkSep,
-- getRest,
-- getWord,
-- parseWord,
-- parseMyString,
-- parseFloat

import Test.HUnit
import Cpt
import ParserBootstrap
import LanguageParser

testCheckSepSuccess :: Test
testCheckSepSuccess = TestCase (assertEqual "test Check Sep Success" (True) (checkSep '('))

testCheckSepSuccess2 :: Test
testCheckSepSuccess2 = TestCase (assertEqual "test Check Sep Success 2" (True) (checkSep ')'))

testCheckSepSuccess3 :: Test
testCheckSepSuccess3 = TestCase (assertEqual "test Check Sep Success 3" (True) (checkSep ' '))

testCheckSepSuccess4 :: Test
testCheckSepSuccess4 = TestCase (assertEqual "test Check Sep Success 4" (True) (checkSep ';'))

testCheckSepFailure :: Test
testCheckSepFailure = TestCase (assertEqual "test Check Sep Success 4" (False) (checkSep 'i'))

testGetWordSuccess :: Test
testGetWordSuccess = TestCase (assertEqual "test Get Word Success" ("if") (getWord "if (True)"))

testGetWordFailure :: Test
testGetWordFailure = TestCase (assertEqual "test Get Word Failure" ("") (getWord ""))

testGetRestSuccess :: Test
testGetRestSuccess = TestCase (assertEqual "test Get Rest Success" (" (True)") (getRest "if (True)"))

testGetRestFailure :: Test
testGetRestFailure = TestCase (assertEqual "test Get Rest Failure" ("") (getRest ""))

testParseWordSuccess :: Test
testParseWordSuccess = TestCase (assertEqual "test Parse Word Success" (Just(VarString "if", " (True)")) (runParser (parseWord "if") "if (True)"))

testParseWordFailure :: Test
testParseWordFailure = TestCase (assertEqual "test Parse Word Failure" (Nothing) (runParser (parseWord "if") "(True)"))

testParseMyStringSuccess :: Test
testParseMyStringSuccess = TestCase (assertEqual "test Parse My String Success" (Just(VarString "if", " (True)")) (runParser parseMyString "if (True)"))

testParseMyStringFailure :: Test
testParseMyStringFailure = TestCase (assertEqual "test Parse My String Failure" (Nothing) (runParser parseMyString ""))

testParseFloatSuccess :: Test
testParseFloatSuccess = TestCase (assertEqual "test Parse Float Success" (Just(VarFloat 12.12, " dada")) (runParser parseFloat "12.12 dada"))

testParseFloatFailure :: Test
testParseFloatFailure = TestCase (assertEqual "test Parse Float Failure" (Nothing) (runParser parseFloat "fzfrzfgzr"))

testParseFloatFailure2 :: Test
testParseFloatFailure2 = TestCase (assertEqual "test Parse Float Failure 2" (Nothing) (runParser parseFloat ""))

testParseEmptySpacesSuccess :: Test
testParseEmptySpacesSuccess = TestCase(assertEqual "test Parse Empty Spaces Success" (Just(" \n\t\n", "string")) (runParser parseEmptySpaces " \n\t\nstring"))

testParseEmptySpacesFailure :: Test
testParseEmptySpacesFailure = TestCase(assertEqual "test Parse Empty Spaces Failure" (Nothing) (runParser (parseEmptySpaces) "string"))

testParseOperatorSuccess :: Test
testParseOperatorSuccess = TestCase(assertEqual "test Parse Operator Success" (Just(VarString "+-|", "fezfezf")) (runParser parseOperator "+-|fezfezf"))

testParseOperatorFailure :: Test
testParseOperatorFailure = TestCase(assertEqual "test Parse Operator Failure" (Nothing) (runParser parseOperator "fezeze"))

testParseOperatorFailure2 :: Test
testParseOperatorFailure2 = TestCase(assertEqual "test Parse Operator Failure 2" (Nothing) (runParser parseOperator ""))

testParserFunctions :: Test
testParserFunctions = TestList [
    TestLabel "test Check Sep success" testCheckSepSuccess, TestLabel "test check Sep success 2" testCheckSepSuccess2, TestLabel "test check Sep success 3" testCheckSepSuccess3, TestLabel "test check Sep success 4" testCheckSepSuccess4, TestLabel "test check Sep failure" testCheckSepFailure
    , TestLabel "test Get Word success" testGetWordSuccess, TestLabel "test Get Word failure" testGetWordFailure
    , TestLabel "test Get Rest sucees" testGetRestSuccess, TestLabel "test Get Rest failure" testGetRestFailure
    , TestLabel "test Parse Word success" testParseWordSuccess, TestLabel "test Parse Word failure" testParseWordFailure
    , TestLabel "test Parse My String success" testParseMyStringSuccess, TestLabel "test Parse My String failure" testParseMyStringFailure
    , TestLabel "test Parse Float success" testParseFloatSuccess, TestLabel "test Parse Float failure" testParseFloatFailure, TestLabel "test Parse Float failure 2" testParseFloatFailure2
    , TestLabel "test Parse Empty Spaces success" testParseEmptySpacesSuccess, TestLabel "test Parse Empty Spaces failure" testParseEmptySpacesFailure
    , TestLabel "test Parse Operator success" testParseOperatorSuccess, TestLabel "test Parse Operator failure" testParseOperatorFailure, TestLabel "test Parse Operator failure 2" testParseOperatorFailure2
    ]
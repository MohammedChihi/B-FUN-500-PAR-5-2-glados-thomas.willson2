module ParserBootstrapTests (
    testParserBootstrapTests
) where

import Test.HUnit
import ParserBootstrap

-- * Test Parse Char * --

testParseCharSuccess :: Test
testParseCharSuccess = TestCase (assertEqual "test Parse Char Success" (Just('a', "bcd")) (runParser (parseChar 'a') "abcd"))

testParseCharFailure :: Test
testParseCharFailure = TestCase (assertEqual "test Parse Char Failure" Nothing (runParser (parseChar 'z') "abcd"))

testParseCharFailure2 :: Test
testParseCharFailure2 = TestCase (assertEqual "test Parse Char Failure 2" Nothing (runParser (parseChar 'z') ""))

-- * Test ParseAnyChar * --

testParseAnyCharSuccess :: Test
testParseAnyCharSuccess = TestCase (assertEqual "test Parse Any Char Success" (Just ('a', "bcd")) (runParser (parseAnyChar "bca") "abcd"))

testParseAnyCharFailure :: Test
testParseAnyCharFailure = TestCase (assertEqual "test Parse Any Char Failure" (Nothing) (runParser (parseAnyChar "xyz") "abcd"))

-- * Test ParseOr * --

testParseOrSuccess :: Test
testParseOrSuccess = TestCase (assertEqual "test Parse Or Succes" (Just ('b', "cda")) (runParser (parseOr (parseChar 'a')  (parseChar 'b')) "bcda"))

testParseOrSuccess2 :: Test
testParseOrSuccess2 = TestCase (assertEqual "test Parse Or Succes 2" (Just ('a', "bcd")) (runParser (parseOr (parseChar 'a')  (parseChar 'z')) "abcd"))

testParseOrFailure :: Test
testParseOrFailure = TestCase (assertEqual "test Parse Or Failure" (Nothing) (runParser (parseOr (parseChar 'a')  (parseChar 'b')) "xyz"))

-- * Test ParseAnd * --

testParseAndSuccess :: Test
testParseAndSuccess = TestCase (assertEqual "test Parse And Success" (Just (('a','b') , "cd")) (runParser (parseAnd (parseChar 'a') ((parseChar 'b'))) "abcd"))

testParseAndFailure :: Test
testParseAndFailure = TestCase (assertEqual "test Parse And Failure" (Nothing) (runParser (parseAnd (parseChar 'a') ((parseChar 'b'))) "bcda"))

-- * Test ParseAndWith * --

testParseAndWithSuccess :: Test
testParseAndWithSuccess = TestCase (assertEqual "test Parser And With Success" (Just ("ab", "cd")) (runParser (parseAndWith (\ x y -> [x , y ]) (parseChar 'a') (parseChar 'b')) "abcd"))

testParseAndWithFailure :: Test
testParseAndWithFailure = TestCase (assertEqual "test Parser And With Failure" (Nothing) (runParser (parseAndWith (\ x y -> [x , y ]) (parseChar 'a') (parseChar 'b')) "xzy"))

-- * Test parseMany * --

testParseManySuccess :: Test
testParseManySuccess = TestCase (assertEqual "test Parse Many Success" (Just ("    ", "foobar")) (runParser (parseMany (parseChar ' ')) "    foobar"))

testParseManySuccess2 :: Test
testParseManySuccess2 = TestCase (assertEqual "test Parse Many Success 2" (Just (" ", "")) (runParser (parseMany (parseChar ' ')) " "))

testParseManyFailure :: Test
testParseManyFailure = TestCase (assertEqual "test Parse Many Failure" (Just("", "    foobar")) (runParser (parseMany (parseChar 'w')) "    foobar"))

-- * Test parseSome * --

testParseSomeSucess :: Test
testParseSomeSucess = TestCase (assertEqual "test Parse Some Success" (Just ("42", "foobar")) (runParser (parseSome (parseAnyChar  ['0'..'9'])) "42foobar"))

testParseSomeFailure :: Test
testParseSomeFailure = TestCase (assertEqual "test Parse Some Failure" (Nothing) (runParser (parseSome (parseAnyChar  ['0'..'9'])) "foobar42"))

-- * Test ParseUInt * --

testParseUIntSucess :: Test
testParseUIntSucess = TestCase (assertEqual "test Parse UInt Success" (Just (42, "fzefezf")) (runParser (parseUInt) "42fzefezf"))

testParseUIntFailure :: Test
testParseUIntFailure = TestCase (assertEqual "test Parse UInt Failure" (Nothing) (runParser (parseUInt) "fzefezf"))

testParseUIntFailure2 :: Test
testParseUIntFailure2 = TestCase (assertEqual "test Parse UInt Failure 2" (Nothing) (runParser (parseUInt) ""))

-- * Test ParseInt * --

testParseIntSucces :: Test
testParseIntSucces = TestCase (assertEqual "test Parse Int Success" (Just(-42, "")) (runParser (parseInt) "-42"))

testParseIntFailure :: Test
testParseIntFailure = TestCase (assertEqual "test Parse Int Failure" (Nothing) (runParser (parseInt) "aaa"))

testParseIntFailure2 :: Test
testParseIntFailure2 = TestCase (assertEqual "test Parse Int Failure 2" (Nothing) (runParser (parseInt) ""))

-- * Test ParseList * --

testParseListSuccess :: Test
testParseListSuccess = TestCase (assertEqual "test Parse List Success" (Just ([1 ,2 ,3 ,5 ,7 ,11 ,13 ,17] , "")) (runParser (parseList parseInt) "(1 2 3 5 7 11 13 17)"))

testParseListFailure :: Test
testParseListFailure = TestCase (assertEqual "test Parse List Failure" (Nothing) (runParser (parseList parseInt) "(a b c d e f)"))

-- * Run Tests * --

testParserBootstrapTests :: Test
testParserBootstrapTests = TestList [
    TestLabel "test Parse Char success" testParseCharSuccess, TestLabel "test Parse Char failure" testParseCharFailure, TestLabel "test Parse Char failure 2" testParseCharFailure2
    , TestLabel "test Parse Any Char success" testParseAnyCharSuccess, TestLabel "test Parse Any Char failure" testParseAnyCharFailure
    , TestLabel "test Parse Or success" testParseOrSuccess, TestLabel "test Parse Or success 2" testParseOrSuccess2, TestLabel "test Parse Or failure" testParseOrFailure
    , TestLabel "test Parse And success" testParseAndSuccess, TestLabel "test Parse And failure" testParseAndFailure
    , TestLabel "test Parse And With success" testParseAndWithSuccess, TestLabel "test Parse And With failure" testParseAndWithFailure
    , TestLabel "test Parse Many success" testParseManySuccess, TestLabel "test Parse Many success 2" testParseManySuccess2, TestLabel "test Parse Many failure" testParseManyFailure
    , TestLabel "test Parse Some success" testParseSomeSucess, TestLabel "test Parse Some failure" testParseSomeFailure
    , TestLabel "test Parse UInt success" testParseUIntSucess, TestLabel "test Parse UInt failure" testParseUIntFailure, TestLabel "test Parse UInt failure 2" testParseUIntFailure2
    , TestLabel "test Parse Int success" testParseIntSucces, TestLabel "test Parse Int failure" testParseIntFailure, TestLabel "test Parse Int failure 2" testParseIntFailure2
    , TestLabel "test Parse List success" testParseListSuccess, TestLabel "test Parse List failure" testParseListFailure
    ]

module Test where 

import Parser
import Lexer
import Test.HUnit
import Control.Monad

shouldPass = runTestTT passingTests
shouldFailParse = runTestTT parseErrors

passWithoutEOF = TestCase (do
  parseOutput <- lexAndParse "pass1.alan"
  assertErrors "pass1 failed" parseOutput) 

pass = TestCase (do
  parseOutput <- lexAndParse "pass2.alan"
  assertErrors "pass2 failed" parseOutput) 

fail1 = TestCase (do
  parseOutput <- lexAndParse "fail1.alan"
  assertErrors "test1 passes" parseOutput)

fail2 = TestCase (do
  parseOutput <- lexAndParse "fail2.alan"
  assertErrors "test2 passes" parseOutput)

fail3 = TestCase (do
  parseOutput <- lexAndParse "fail3.alan"
  assertErrors "test3 passes" parseOutput)

fail4 = TestCase (do
  parseOutput <- lexAndParse "fail4.alan"
  assertErrors "test4 passes" parseOutput)

fail5 = TestCase (do
  parseOutput <- lexAndParse "fail5.alan"
  assertErrors "test5 passes" parseOutput)

fail6 = TestCase (do
  parseOutput <- lexAndParse "fail6.alan"
  assertErrors "test6 passes" parseOutput)

fail7 = TestCase (do
  parseOutput <- lexAndParse "fail7.alan"
  assertErrors "test7 passes" parseOutput)

fail8 = TestCase (do
  parseOutput <- lexAndParse "fail8.alan"
  assertErrors "test8 passes" parseOutput)

assertErrors s list = unless ([] == list) (assertFailure s)

lexAndParse file = do
  rawCode <- readFile file 
  let tokens = Lexer.lex rawCode
  debugPrint tokens
  let result = (third (parse tokens))
  return result
  
third (_,_,x) = x
second (_,x,_) = x
passingTests = TestList [TestLabel "passing without a $" passWithoutEOF, TestLabel "passing with stuff" pass]
parseErrors = TestList[TestLabel "failing with uneven brackets" fail1,
                  TestLabel "failing with uneven brackets" fail2,
                  TestLabel "failing with uneven brackets" fail4,
                  TestLabel "failing with uneven brackets" fail5,
                  TestLabel "failing with uneven brackets" fail6,
                  TestLabel "failing with uneven brackets" fail7,
                  TestLabel "failing with uneven brackets" fail8]
                  

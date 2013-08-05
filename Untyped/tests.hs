{--| Tests for Lambda Calculus Interpreter

James Wang, 27 Jun 2013

--}

module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HU

import Untyped.Interpreter

main :: IO ()
main = defaultMain tests

lexerSimpleTest :: HU.Test
lexerSimpleTest =
    "Simple test" HU.~: lexer "(lambda x x)" HU.@=? ["(", "lambda", "x", "x", ")"]

lexerNestedTest :: HU.Test
lexerNestedTest =
    "Nested test" HU.~: lexer "(lambda x (lambda x y))" HU.@=? ["(", "lambda", "x",
                                                                "(", "lambda", "x",
                                                                "y", ")", ")"]

{--
lexerEmptyTest :: String -> [String]


lexerComplexTest :: String -> [String]


parserSimpleTest :: [String] -> Expression
parserSimpleTest =
    "Simple parse" HU.~: parser


parserEmptyTest :: [String] -> Expression


parserComplexTest :: [String] -> Expression


reducerSimpleTest :: Expression -> Expression


reducerEmptyTest :: HU.Test
reducerEmptyTest =
  "reducer test" HU.~: ___ HU.@=? ___

reducerComplexTest :: Expression -> Expression
--}

tests :: [Test]
tests =
  [
    testGroup "Lexer"
    [
      -- testProperty "Simple test" lexerSimpleTest
      testGroup "Unit test for lexer (simple)" $ hUnitTestToTests lexerSimpleTest,
      testGroup "Unit test for lexer (nested)" $ hUnitTestToTests lexerNestedTest

    ]
{--
    testGroup "Parser"
    [


    ]
    testGroup "Reducer"
    [
      testGroup "Unit test for empty" $ hUnitTestToTests reducerEmptyTest

    ]
--}
  ]

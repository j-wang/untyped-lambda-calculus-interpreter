{--| Tests for Lambda Calculus Interpreter

James Wang, 27 Jun 2013

--}

module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HU

import Interpreter

main :: IO ()
main = defaultMain Tests

lexerSimpleTest :: HU.Test
lexerSimpleTest =
  "Simple test" HU.~: "(lambda x x)" HU.@=? [["lambda", "x", "x"]]

lexerNestedTest :: HU.Test
lexerNestedTest =
  "Nested test" HU.~: "(lambda x (lambda x y))" HU.@=? [["lambda", "x", ["lambda", "x", "y"]]]

lexerEmptyTest :: String -> [String]


lexerComplexTest :: String -> [String]


parserSimpleTest :: [String] -> Expression


parserEmptyTest :: [String] -> Expression


parserComplexTest :: [String] -> Expression


reducerSimpleTest :: Expression -> Expression


reducerEmptyTest :: HU.Test
reducerEmptyTest =
  "reducer test" HU.~: ___ HU.@=? ___

reducerComplexTest :: Expression -> Expression


tests :: [Test]
tests =
  [
    testGroup "Lexer"
    [
      testProperty "Simple test" lexerSimpleTest

    ]
    testGroup "Parser"
    [


    ]
    testGroup "Reducer"
    [
      testGroup "Unit test for empty" $ hUnitTestToTests reducerEmptyTest

    ]
  ]

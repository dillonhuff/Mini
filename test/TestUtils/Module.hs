module TestUtils.Module(testFunction,
                        testFunctionIO) where

import Control.Monad
import Data.List as L
import Test.HUnit

testFunction func cases = runTestTT $ makeTestCases func cases

makeTestCases func cases =
  TestList $ map (\(input, expected) -> testCase func input expected) cases

testCase func input expected =
  TestCase (assertEqual ("Input: " ++ show input) expected (func input))

testFunctionIO func cases = runTestTT $ TestList $ L.map (ioTestCase func) cases

ioTestCase :: (Show a, Show b, Eq b) => (a -> IO b) -> (a, b) -> Test
ioTestCase f (input, expected) =
  TestCase $ ioAssert f input expected

ioAssert :: (Show a, Show b, Eq b) => (a -> IO b) -> a -> b -> Assertion
ioAssert f input expected = do
  actual <- f input
  assertEqual ("Input: " ++ show input) expected actual


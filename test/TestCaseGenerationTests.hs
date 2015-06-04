module TestCaseGenerationTests(allTestCaseGenerationTests) where

import Control.Monad.Random
import Data.List as L

import IndexExpression
import SymbolTable
import Testing.TestCaseGeneration
import TestUtils

allTestCaseGenerationTests :: IO ()
allTestCaseGenerationTests = do
  r1 <- testFunctionM (testGenTestCases (3 :: Int) (100 :: Int)) symtabCases
  putStrLn r1

symtabCases =
  L.map (\(x, y) -> (createSymtabFromTuple x, y))
  [([("A", iVar "m", iVar "n", iVar "rs", iVar "cs")], True),
   ([("x", iVar "m", iConst 1, iConst 1, iConst 1)], True),
   ([("x", iVar "m", iConst 1, iConst 1, iConst 1),
     ("a", iConst 1, iConst 1, iConst 1, iConst 1)], True),
   ([("A", iVar "m", iVar "n", iVar "rs", iConst 1)], True),
   ([("A", iVar "m", iVar "n", iConst 1, iVar "cs")], True)]

createSymtabFromTuple namesAndLayouts =
  mOpSymtab $ L.map (\(name, nr, nc, rs, cs) -> (name, mOpSymInfo arg doubleFloat (layout nr nc rs cs))) namesAndLayouts

testGenTestCases :: (MonadRandom m, Random a, Num a) => a -> a -> MOpSymtab -> m Bool
testGenTestCases lo hi st = do
  cases <- genTestCases lo hi st
  case cases of
    [] -> return False
    other -> return True

module Testing.TestCaseGenerationTests(allTestCaseGenerationTests) where

import Control.Monad.Random
import Data.List as L

import Core.IndexExpression
import Core.SymbolTable
import Testing.TestCaseGeneration
import TestUtils.Module

allTestCaseGenerationTests =
  testFunctionIO (testGenTestCases (3 :: Int) (100 :: Int)) symtabCases

symtabCases =
  L.map (\(x, y) -> (createSymtabFromTuple x, y))
  [([("A", iVar "m", iVar "n", iVar "rs", iVar "cs")], True),
   ([("x", iVar "m", iConst 1, iConst 1, iConst 1)], True),
   ([("x", iVar "m", iConst 1, iConst 1, iConst 1),
     ("a", iConst 1, iConst 1, iConst 1, iConst 1)], True),
   ([("A", iVar "m", iVar "n", iVar "rs", iConst 1)], True),
   ([("A", iVar "m", iVar "n", iConst 1, iVar "cs")], True),
   ([("A", iVar "m", iVar "n", iConst 1, iVar "cs"),
     ("x", iConst 1, iConst 1, iConst 1, iConst 1)], True),
   ([("A", iVar "m", iVar "n", iVar "Ars", iConst 1),
     ("B", iVar "m", iVar "n", iVar "Brs", iConst 1)], True),
   ([("x", iVar "n", iConst 1, iConst 1, iConst 1),
     ("A", iVar "m", iVar "n", iConst 1, iVar "Ars"),
     ("y", iVar "n", iConst 1, iConst 1, iConst 1)], True),
   ([("A", iVar "m", iVar "n", iVar "Ars", iConst 1),
     ("B", iVar "m", iVar "n", iConst 1, iVar "Bcs")], False),
   ([("a", iConst 1, iConst 1, iConst 1, iConst 1),
     ("x", iVar "n", iConst 1, iConst 1, iConst 1),
     ("A", iVar "m", iVar "n", iConst 1, iVar "Ars"),
     ("y", iVar "n", iConst 1, iConst 1, iConst 1)], True)]

createSymtabFromTuple namesAndLayouts =
  mOpSymtab $ L.map (\(name, nr, nc, rs, cs) -> (name, mOpSymInfo arg doubleFloat (layout nr nc rs cs))) namesAndLayouts

testGenTestCases :: (Random a, Num a, Show a) => a -> a -> MOpSymtab -> IO Bool
testGenTestCases lo hi st = do
  cases <- genTestCases lo hi st
  case cases of
    [] -> return False
    other -> do
      return True

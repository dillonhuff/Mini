module DependenceAnalysisTests(allDependenceAnalysisTests) where

import Data.List as L

import Analysis.Dependence.Graph
import Analysis.Dependence.Register
import Core.IndexExpression
import Core.MiniOperation
import Core.SymbolTable
import Core.MiniSyntax
import TestUtils

allDependenceAnalysisTests = do
  testFunction isFlowDependentTest flowDependentCases
  testFunction isAntiDependentTest antiDependentCases
  testFunction isOutputDependentTest outputDependentCases
  testFunction isInputDependentTest inputDependentCases
  testFunction grSimpleFlowTest grSimpleFlowCases
  testFunction grSimpleAntiTest grSimpleAntiCases
  testFunction grSimpleOutTest grSimpleOutCases

flowDependentCases =
  [((plus "a" "b" "c" 0, minus "x" "y" "z" 1), False),
   ((times "x" "a" "c" 1, loadConst "a" (doubleLit 12.2) 0), True),
   ((regAssign "a" "a" 1, plus "a" "b" "c" 0), True),
   ((regAssign "a" "c" 0, plus "c" "x" "d" 1), True),
   ((regAssign "a" "c" 0, minus "a" "x" "d" 1), False),
   ((regAssign "x" "a" 1, regAssign "a" "b" 0), True),
   ((regAssign "x" "b" 1, regAssign "a" "b" 0), False)]

antiDependentCases =
  [((minus "x" "y" "z" 1, plus "a" "b" "c" 0), False),
   ((times "x" "a" "c" 1, plus "k" "b" "x" 0), True),
   ((plus "a" "b" "c" 0, regAssign "a" "a" 1), True),
   ((plus "a" "b" "c" 0, regAssign "x" "a" 1), True),
   ((regAssign "a" "b" 0, regAssign "x" "a" 1), True),
   ((regAssign "a" "b" 0, regAssign "x" "b" 1), False)]

outputDependentCases =
  [((plus "a" "b" "c" 0, minus "c" "a" "d" 1), False),
   ((plus "a" "b" "c" 0, plus "a" "x" "u" 1), True),
   ((regAssign "a" "c" 0, minus "a" "x" "d" 1), True),
   ((regAssign "a" "b" 0, regAssign "a" "c" 1), True),
   ((regAssign "a" "a" 0, regAssign "a" "a" 1), True)]

inputDependentCases =
  [((plus "a" "b" "c" 0, times "x" "y" "z" 1), False),
   ((plus "a" "b" "c" 0, times "x" "y" "c" 1), True),
   ((plus "a" "b" "c" 0, times "x" "b" "c" 1), True),
   ((regAssign "x" "c" 0, regAssign "y" "c" 1), True)]

grSimpleFlowCases =
  [((0, 1), False),
   ((1, 0), True),
   ((1, 2), False),
   ((2, 1), True),
   ((0, 2), False),
   ((2, 0), True)]

grSimpleAntiCases =
  [((1, 0), False),
   ((0, 1), False),
   ((3, 1), True)]

grSimpleOutCases =
  [((1, 0), False),
   ((3, 0), True)]
   
isFlowDependentTest (s1, s2) = isFlowDependent s1 s2
isAntiDependentTest (s1, s2) = isAntiDependent s1 s2
isOutputDependentTest (s1, s2) = isOutputDependent s1 s2
isInputDependentTest (s1, s2) = isInputDependent s1 s2

grSimpleFlowTest (t, s) =
  flowDependent simpleGraph t s

grSimpleAntiTest (t, s) =
  antiDependent simpleGraph t s

grSimpleOutTest (t, s) =
  outputDependent simpleGraph t s

simpleGraph =
  case registerDependenceGraph [loadConst "a" (doubleLit 1.0) 0, regAssign "b" "a" 1, times "x" "a" "b" 2, plus "a" "b" "x" 3] of
    Just g -> g

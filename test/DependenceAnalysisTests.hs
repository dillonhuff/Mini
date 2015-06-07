module DependenceAnalysisTests(allDependenceAnalysisTests) where

import Data.List as L

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

flowDependentCases =
  [((plus "a" "b" "c" 0, minus "x" "y" "z" 1), False),
   ((times "x" "a" "c" 1, loadConst "a" (doubleLit 12.2) 0), True),
   ((store "a" (iVar "i") "a" 1, plus "a" "b" "c" 0), True),
   ((regAssign "a" "c" 0, plus "c" "x" "d" 1), True),
   ((regAssign "a" "c" 0, minus "a" "x" "d" 1), False),
   ((load "x" "a" (iVar "j") 1, store "a" (iVar "i") "b" 0), True),
   ((load "x" "b" (iVar "j") 1, store "a" (iVar "i") "b" 0), False)]

antiDependentCases =
  [((minus "x" "y" "z" 1, plus "a" "b" "c" 0), False),
   ((times "x" "a" "c" 1, plus "k" "b" "x" 0), True),
   ((plus "a" "b" "c" 0, store "a" (iVar "i") "a" 1), True),
   ((plus "a" "b" "c" 0, regAssign "x" "a" 1), True),
   ((store "a" (iVar "i") "b" 0, load "x" "a" (iVar "j") 1), True),
   ((store "a" (iVar "i") "b" 0, load "x" "b" (iVar "j") 1), False)]

outputDependentCases =
  [((plus "a" "b" "c" 0, minus "c" "a" "d" 1), False),
   ((plus "a" "b" "c" 0, plus "a" "x" "u" 1), True),
   ((regAssign "a" "c" 0, minus "a" "x" "d" 1), True),
   ((store "a" (iVar "j") "b" 0, store "a" (iVar "k") "c" 1), True),
   ((load "a" "a" (iVar "k") 1, store "a" (iVar "k") "a" 1), False)]

inputDependentCases =
  [((plus "a" "b" "c" 0, times "x" "y" "z" 1), False),
   ((plus "a" "b" "c" 0, times "x" "y" "c" 1), True),
   ((plus "a" "b" "c" 0, times "x" "b" "c" 1), True),
   ((store "x" (iVar "l") "c" 0, store "y" (iVar "n") "c" 1), True)]
  
isFlowDependentTest (s1, s2) = isFlowDependent s1 s2
isAntiDependentTest (s1, s2) = isAntiDependent s1 s2
isOutputDependentTest (s1, s2) = isOutputDependent s1 s2
isInputDependentTest (s1, s2) = isInputDependent s1 s2

queryFlowDependencies (depGraph, l1, l2) =
  flowDependent depGraph l1 l2

queryAntiDependencies (depGraph, l1, l2) =
  antiDependent depGraph l1 l2


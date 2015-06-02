module DependenceAnalysisTests(allDependenceAnalysisTests) where

import Data.List as L

import DependenceAnalysis
import IndexExpression
import Syntax
import TestUtils

allDependenceAnalysisTests = do
  testFunction isFlowDependentTest noIndexFlowDependentCases
  testFunction isFlowDependentTest indexFlowDependentCases

noIndexFlowDependentCases =
  L.map (\((x, y), z) -> (([], x, y), z))
  [((plus "a" "b" "c" 0, minus "x" "y" "z" 1), False),
   ((loadConst "a" (doubleLit 12.2) 0, times "x" "a" "c" 1), True),
   ((plus "a" "b" "c" 0, store "a" (iVar "i") "a" 1), True)]

indexFlowDependentCases =
  [(([indexRange (iConst 0) (iVar "n"), indexRange (iConst 0) (iVar "k")], store "a" (iVar "i") "b" 0, load "x" "a" (iVar "j") 1), True),
   (([indexRange (iConst 0) (iVar "n"), indexRange (iConst 0) (iVar "k")], store "a" (iVar "i") "b" 0, load "x" "b" (iVar "j") 1), False)]

isFlowDependentTest (i, s1, s2) = isFlowDependent i s1 s2

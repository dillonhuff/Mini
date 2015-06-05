module DependenceAnalysisTests(allDependenceAnalysisTests) where

import Data.List as L

import Analysis.DependenceAnalysis
import IndexExpression
import MiniOperation
import SymbolTable
import Syntax
import TestUtils

allDependenceAnalysisTests = do
  testFunction isFlowDependentTest noIndexFlowDependentCases
  testFunction isFlowDependentTest indexFlowDependentCases
  testFunction isAntiDependentTest noIndexAntiDependentCases
  testFunction isAntiDependentTest indexAntiDependentCases
  testFunction isOutputDependentTest noIndexOutputDependentCases
  testFunction isInputDependentTest noIndexInputDependentCases
  testFunction queryFlowDependencies wholeOperationFlowDepsCases
  testFunction queryAntiDependencies wholeOperationAntiDepsCases

noIndexFlowDependentCases =
  L.map (\((x, y), z) -> (([], x, y), z))
  [((plus "a" "b" "c" 0, minus "x" "y" "z" 1), False),
   ((loadConst "a" (doubleLit 12.2) 0, times "x" "a" "c" 1), True),
   ((plus "a" "b" "c" 0, store "a" (iVar "i") "a" 1), True)]

indexFlowDependentCases =
  [((dummyRange, store "a" (iVar "i") "b" 0, load "x" "a" (iVar "j") 1), True),
   ((dummyRange, store "a" (iVar "i") "b" 0, load "x" "b" (iVar "j") 1), False)]

noIndexAntiDependentCases =
  L.map (\((x, y), z) -> (([], x, y), z))
  [((plus "a" "b" "c" 0, minus "x" "y" "z" 1), False),
   ((plus "k" "b" "x" 0, times "x" "a" "c" 1), True),
   ((store "a" (iVar "i") "a" 1, plus "a" "b" "c" 0), True)]

indexAntiDependentCases =
  [((dummyRange, load "x" "a" (iVar "j") 1, store "a" (iVar "i") "b" 0), True),
   ((dummyRange, load "x" "b" (iVar "j") 1,  store "a" (iVar "i") "b" 0), False)]

noIndexOutputDependentCases =
  L.map (\((x, y), z) -> (([], x, y), z))
  [((plus "a" "b" "c" 0, minus "c" "a" "d" 1), False),
   ((plus "a" "b" "c" 0, plus "a" "x" "u" 1), True),
   ((store "a" (iVar "j") "b" 0, store "a" (iVar "k") "c" 1), True),
   ((load "a" "a" (iVar "k") 1, store "a" (iVar "k") "a" 1), False)]

noIndexInputDependentCases =
  L.map (\((x, y), z) -> (([], x, y), z))
  [((plus "a" "b" "c" 0, times "x" "y" "z" 1), False),
   ((plus "a" "b" "c" 0, times "x" "y" "c" 1), True),
   ((plus "a" "b" "c" 0, times "x" "b" "c" 1), True),
   ((store "x" (iVar "l") "c" 0, store "y" (iVar "n") "c" 1), True)]
  
isFlowDependentTest (i, s1, s2) = isFlowDependent i s1 s2
isAntiDependentTest (i, s1, s2) = isAntiDependent i s1 s2
isOutputDependentTest (i, s1, s2) = isOutputDependent i s1 s2
isInputDependentTest (i, s1, s2) = isInputDependent i s1 s2

dummyRange =
  [indexRange (iConst 0) (iVar "n"), indexRange (iConst 0) (iVar "k")]

wholeOperationFlowDepsCases =
  L.map (\((x, y), z) -> ((opDepGraph, x, y), z))
  [(("l1", "l2"), False),
   (("l2", "l3"), True)]

wholeOperationAntiDepsCases =
  L.map (\((x, y), z) -> ((opDepGraph, x, y), z))
  [(("l1", "l2"), False),
   (("l2", "l3"), False)]
--   (("l3", "l2"), True)]

queryFlowDependencies (depGraph, l1, l2) =
  flowDependent depGraph l1 l2

queryAntiDependencies (depGraph, l1, l2) =
  antiDependent depGraph l1 l2

opDepGraph = dependenceGraph testOperation

testOperation = operation "test" (miniSymtab []) $ block [alphaLoop, sumLoop, copyLoop]

alphaLoop = for "i" (iConst 0) (iSub (iVar "n") (iConst 1)) (iAdd (iVar "i") (iConst 1)) (block alphaBody) "l0"

alphaBody =
  [load "alpha20" "alpha" (iConst 0) "l1",
   load "x21" "x" (iVar "i") "l2",
   times "tmp022" "alpha20" "x21" "l3"]

sumLoop = for "k" (iConst 0) (iSub (iVar "n") (iConst 1)) (iAdd (iVar "k") (iConst 1)) (block []) "l20"

copyLoop = for "l" (iConst 0) (iSub (iVar "n") (iConst 1)) (iAdd (iVar "l") (iConst 1)) (block []) "l40"

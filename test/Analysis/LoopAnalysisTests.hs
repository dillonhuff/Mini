module Analysis.LoopAnalysisTests(allLoopAnalysisTests) where

import Data.List as L

import Analysis.Loop
import Core.IndexExpression
import TestUtils.Dummies.Loop
import TestUtils.Module

allLoopAnalysisTests = do
  testFunction numberOfIterationsFromTriple numItersSuccessCases
  testFunction numberOfIterationsFromTriple numItersFailureCases
  testFunction allIterationsListFromTriple allIterationsSuccessCases
  testFunction finalIndexValue finalIndexValueCases

numItersSuccessCases =
  L.map (\(x, y) -> (x, Just y))
  [((iConst 0, iConst 1, iConst 0), 1),
   ((iConst 1, iConst 3, iConst 17), 6),
   ((iAdd (iConst 1) (iConst 1), iMul (iConst 1) (iConst 2), iMul (iConst 4) (iConst 3)), 6)]

numItersFailureCases =
  L.map (\x -> (x, Nothing))
  [(iConst 1, iConst 1, iVar "a"),
   (iConst 1, iVar "a", iConst 34),
   (iVar "a", iConst 1, iConst 4)]

allIterationsSuccessCases =
  L.map (\(x, y) -> (x, Just y))
  [((iConst 0, iConst 1, iConst 0), [iConst 0]),
   ((iConst 3, iConst 3, iConst 17), [iConst 3, iConst 6, iConst 9, iConst 12, iConst 15])]

numberOfIterationsFromTriple (start, inc, end) = numberOfIterations start inc end
allIterationsListFromTriple (start, inc, end) = allIterationsList start inc end

finalIndexValueCases =
  [(pS0I1ES "i" "n" [], Nothing),
   (pS0I1EC "i" 12 [], Just $ iConst 13),
   (pS0ICEC "i" 5 17 [], Just $ iConst 20),
   (pSCICEC "i" 5 3 16 [], Just $ iConst 17)]

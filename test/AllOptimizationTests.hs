module AllOptimizationTests(main) where

import Optimizations.ArrayCompactionTests
import Optimizations.LoopFusionTests
import Optimizations.PartialLoopUnrollingTests
import Optimizations.TempBufferEliminationTests

main = do
  allArrayCompactionTests
  allLoopFusionTests
  allPartialLoopUnrollingTests
  allTempBufferEliminationTests

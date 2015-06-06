module AllOptimizationTests(main) where

import ArrayCompactionTests
import FullLoopUnrollingTests
import LoopFusionTests
import PartialLoopUnrollingTests
import TempBufferEliminationTests

main = do
  allArrayCompactionTests
  allFullLoopUnrollingTests
  allLoopFusionTests
  allPartialLoopUnrollingTests
  allTempBufferEliminationTests

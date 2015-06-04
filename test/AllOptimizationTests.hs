module AllOptimizationTests(main) where

import FullLoopUnrollingTests
import LoopFusionTests
import PartialLoopUnrollingTests
import TempBufferEliminationTests

main = do
  allFullLoopUnrollingTests
  allLoopFusionTests
  allPartialLoopUnrollingTests
  allTempBufferEliminationTests

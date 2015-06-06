module AllOptimizationTests(main) where

import ArrayCompactionTests
import CopyPropagationTests
import FullLoopUnrollingTests
import LoopFusionTests
import PartialLoopUnrollingTests
import TempBufferEliminationTests

main = do
  allArrayCompactionTests
  allCopyPropagationTests
  allFullLoopUnrollingTests
  allLoopFusionTests
  allPartialLoopUnrollingTests
  allTempBufferEliminationTests

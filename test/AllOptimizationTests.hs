module AllOptimizationTests(main) where

import Optimizations.ArrayCompactionTests
import Optimizations.CopyPropagationTests
import Optimizations.FullLoopUnrollingTests
import Optimizations.LoopFusionTests
import Optimizations.PartialLoopUnrollingTests
import Optimizations.TempBufferEliminationTests

main = do
  allArrayCompactionTests
  allCopyPropagationTests
  allFullLoopUnrollingTests
  allLoopFusionTests
  allPartialLoopUnrollingTests
  allTempBufferEliminationTests

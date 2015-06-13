module AllSystemTests(main) where

import Core.MOpTests
import Optimizations.ArrayCompactionTests
import Optimizations.LoopFusionTests
import Optimizations.PartialLoopUnrollingTests
import Optimizations.TempBufferEliminationTests

main = do
  allArrayCompactionTests
  allLoopFusionTests
  allMOpTests
  allPartialLoopUnrollingTests
  allTempBufferEliminationTests

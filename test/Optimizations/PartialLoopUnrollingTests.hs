module Optimizations.PartialLoopUnrollingTests(allPartialLoopUnrollingTests) where

import Core.MiniOperation
import Optimizations.FullLoopUnrolling
import Optimizations.IndexExpressionOptimizations
import Optimizations.PartialLoopUnrolling
import SystemSettings
import Testing.LibraryOptimization

libName = "LargeTests"
level1Path = testPath ++ libName ++ ".lspc"
level1CPath = testPath ++ libName ++ ".c"

allPartialLoopUnrollingTests = do
  compileLibSpecToFileWithOptimization (sequenceOptimization "PartialUnrollingTestOpts" [partiallyUnrollAllLoopsBy 4, evalIExprConstants, fullyUnrollAllLoops]) level1Path level1CPath

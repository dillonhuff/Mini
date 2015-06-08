module Optimizations.PartialLoopUnrollingTests(allPartialLoopUnrollingTests) where

import Optimizations.FullLoopUnrolling
import Optimizations.IndexExpressionOptimizations
import Optimizations.PartialLoopUnrolling
import SystemSettings
import Testing.LibraryOptimization

libName = "LargeTests"
level1Path = projectPath ++ libName ++ ".lspc"
level1CPath = projectPath ++ libName ++ ".c"

allPartialLoopUnrollingTests = do
  compileLibSpecToFileWithOptimizations [partiallyUnrollAllLoopsBy 4, evalIExprConstants, fullyUnrollAllLoops] level1Path level1CPath

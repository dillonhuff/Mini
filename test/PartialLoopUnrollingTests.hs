module PartialLoopUnrollingTests(allPartialLoopUnrollingTests) where

import FullLoopUnrolling
import IndexExpressionOptimizations
import PartialLoopUnrolling
import SystemSettings
import Testing.LibraryOptimization

libName = "LargeTests"
level1Path = projectPath ++ libName ++ ".lspc"
level1CPath = projectPath ++ libName ++ ".c"

allPartialLoopUnrollingTests = do
  compileLibSpecToFileWithOptimizations [partiallyUnrollAllLoopsBy 4, evalIExprConstants, fullyUnrollAllLoops] level1Path level1CPath

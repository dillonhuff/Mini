module TempBufferEliminationTests(allTempBufferEliminationTests) where

import Optimizations.FullLoopUnrolling
import Optimizations.IndexExpressionOptimizations
import Optimizations.LoopFusion
import Optimizations.TempBufferElimination
import SystemSettings
import Testing.LibraryOptimization

libName = "LargeTests"
level1Path = projectPath ++ libName ++ ".lspc"
level1CPath = projectPath ++ libName ++ ".c"

allTempBufferEliminationTests =
  compileLibSpecToFileWithOptimizations [eliminateTempBuffers,
                                         fuseAllTopLevelLoopsPossible,
                                         evalIExprConstants,
                                         fullyUnrollAllLoops] level1Path level1CPath

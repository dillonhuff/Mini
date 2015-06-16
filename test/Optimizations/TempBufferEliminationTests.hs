module Optimizations.TempBufferEliminationTests(allTempBufferEliminationTests) where

import Core.MiniOperation
import Optimizations.FullLoopUnrolling
import Optimizations.IndexExpressionOptimizations
import Optimizations.LoopFusion
import Optimizations.TempBufferElimination
import SystemSettings
import Testing.LibraryOptimization

libName = "LargeTests"
level1Path = testPath ++ libName ++ ".lspc"
level1CPath = testPath ++ libName ++ ".c"

allTempBufferEliminationTests =
  compileLibSpecToFileWithOptimization (sequenceOptimization "TempBufferEliminationTestOpts" [eliminateTempBuffers,
                                         fuseAllTopLevelLoopsPossible,
                                         evalIExprConstants,
                                         fullyUnrollAllLoops]) level1Path level1CPath

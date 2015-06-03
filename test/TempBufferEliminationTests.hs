module TempBufferEliminationTests(allTempBufferEliminationTests) where

import FullLoopUnrolling
import IndexExpressionOptimizations
import LoopFusion
import SystemSettings
import TempBufferElimination
import Testing.LibraryOptimization

libName = "Level1BLAS"
level1Path = projectPath ++ libName ++ ".lspc"
level1CPath = projectPath ++ libName ++ ".c"

allTempBufferEliminationTests =
  compileLibSpecToFileWithOptimizations [eliminateTempBuffers,
                                         fuseAllTopLevelLoopsPossible,
                                         evalIExprConstants,
                                         fullyUnrollAllLoops] level1Path level1CPath

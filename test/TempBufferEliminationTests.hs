module TempBufferEliminationTests(allTempBufferEliminationTests) where

import FullLoopUnrolling
import IndexExpressionOptimizations
import LoopFusion
import SystemSettings
import TempBufferElimination
import Testing.LibraryOptimization

level1Path = projectPath ++ "DSwap.lspc"
level1CPath = projectPath ++ "DSwap.c"

allTempBufferEliminationTests =
  compileLibSpecToFileWithOptimizations [eliminateTempBuffers,
                                         fuseAllTopLevelLoopsPossible,
                                         evalIExprConstants,
                                         fullyUnrollAllLoops] level1Path level1CPath

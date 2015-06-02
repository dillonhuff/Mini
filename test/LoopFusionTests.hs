module LoopFusionTests(allLoopFusionTests) where

import FullLoopUnrolling
import IndexExpressionOptimizations
import LoopFusion
import SystemSettings
import Testing.LibraryOptimization

level1Path = projectPath ++ "Level1BLAS.lspc"
level1CPath = projectPath ++ "Level1BLAS.c"

allLoopFusionTests =
  compileLibSpecToFileWithOptimizations [fuseAllTopLevelLoopsPossible, evalIExprConstants, fullyUnrollAllLoops] level1Path level1CPath

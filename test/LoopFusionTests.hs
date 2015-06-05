module LoopFusionTests(allLoopFusionTests) where

import Optimizations.FullLoopUnrolling
import Optimizations.IndexExpressionOptimizations
import Optimizations.LoopFusion
import SystemSettings
import Testing.LibraryOptimization

libName = "LargeTests"
level1Path = projectPath ++ libName ++ ".lspc"
level1CPath = projectPath ++ libName ++ ".c"

allLoopFusionTests =
  compileLibSpecToFileWithOptimizations [fuseAllTopLevelLoopsPossible, evalIExprConstants, fullyUnrollAllLoops] level1Path level1CPath

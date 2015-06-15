module Optimizations.LoopFusionTests(allLoopFusionTests) where

import Core.MiniOperation
import Optimizations.ArrayCompaction
import Optimizations.CopyPropagation
import Optimizations.FullLoopUnrolling
import Optimizations.IndexExpressionOptimizations
import Optimizations.LoopFusion
import SystemSettings
import Testing.LibraryOptimization

libName = "LargeTests"
level1Path = projectPath ++ libName ++ ".lspc"
level1CPath = projectPath ++ libName ++ ".c"

allLoopFusionTests =
  compileLibSpecToFileWithOptimization (sequenceOptimization "LoopFusionTestOpts" [fuseAllTopLevelLoopsPossible,
                                                                                   propagateAllTopLevelCopiesPossible,
                                                                                   compactArrays,
                                                                                   evalIExprConstants,
                                                                                   fullyUnrollAllLoops]) level1Path level1CPath

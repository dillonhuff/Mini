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
level1Path = testPath ++ libName ++ ".lspc"
level1CPath = testPath ++ libName ++ ".c"

allLoopFusionTests =
  compileLibSpecToFileWithOptimization (sequenceOptimization "LoopFusionTestOpts" [fuseAllTopLevelLoopsPossible,
                                                                                   propagateAllTopLevelCopiesPossible,
                                                                                   compactArrays,
                                                                                   evalIExprConstants,
                                                                                   fullyUnrollAllLoops]) level1Path level1CPath

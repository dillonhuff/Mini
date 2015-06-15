module Optimizations.ArrayCompactionTests(allArrayCompactionTests) where

import Core.MiniOperation
import Optimizations.ArrayCompaction
import Optimizations.FullLoopUnrolling
import SystemSettings
import Testing.LibraryOptimization

libName = "LargeTests"
level1Path = projectPath ++ libName ++ ".lspc"
level1CPath = projectPath ++ libName ++ ".c"

allArrayCompactionTests =
  compileLibSpecToFileWithOptimization (sequenceOptimization "ArrayCompactionTestOpts" [compactArrays, fullyUnrollAllLoops]) level1Path level1CPath

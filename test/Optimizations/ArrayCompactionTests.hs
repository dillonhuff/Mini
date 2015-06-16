module Optimizations.ArrayCompactionTests(allArrayCompactionTests) where

import Core.MiniOperation
import Optimizations.ArrayCompaction
import Optimizations.FullLoopUnrolling
import SystemSettings
import Testing.LibraryOptimization

libName = "LargeTests"
level1Path = testPath ++ libName ++ ".lspc"
level1CPath = testPath ++ libName ++ ".c"

allArrayCompactionTests =
  compileLibSpecToFileWithOptimization (sequenceOptimization "ArrayCompactionTestOpts" [compactArrays, fullyUnrollAllLoops]) level1Path level1CPath

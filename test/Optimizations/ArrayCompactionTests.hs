module Optimizations.ArrayCompactionTests(allArrayCompactionTests) where

import Optimizations.ArrayCompaction
import SystemSettings
import Testing.LibraryOptimization

libName = "LargeTests"
level1Path = projectPath ++ libName ++ ".lspc"
level1CPath = projectPath ++ libName ++ ".c"

allArrayCompactionTests =
  compileLibSpecToFileWithOptimizations [compactArrays] level1Path level1CPath

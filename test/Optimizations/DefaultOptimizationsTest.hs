module Optimizations.DefaultOptimizationsTest(defaultOptimizationsTest) where


import BackEnd.RunBackEnd
import SystemSettings
import Testing.LibraryOptimization

libName = "LargeTests"
level1Path = testPath ++ libName ++ ".lspc"
level1CPath = testPath ++ libName ++ ".c"

defaultOptimizationsTest = do
  compileLibSpecToFileWithOptimizations defaultOptimizations level1Path level1CPath

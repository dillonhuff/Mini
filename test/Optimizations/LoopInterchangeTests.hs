module Optimizations.LoopInterchangeTests(allLoopInterchangeTests) where

import BackEnd.RunBackEnd
import Core.MiniOperation
import Optimizations.LoopInterchange
import SystemSettings
import Testing.LibraryOptimization

libName = "LargeTests"
level1Path = projectPath ++ libName ++ ".lspc"
level1CPath = projectPath ++ libName ++ ".c"

allLoopInterchangeTests =
  compileLibSpecToFileWithOptimization (sequenceOptimization "LoopInterchangeTestOpts"
                                                             [interchangeLoops,
                                                              cleanupOperation])
                                                              level1Path level1CPath

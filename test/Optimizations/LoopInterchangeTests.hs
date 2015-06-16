module Optimizations.LoopInterchangeTests(allLoopInterchangeTests) where

import BackEnd.RunBackEnd
import Core.MiniOperation
import Optimizations.LoopInterchange
import Optimizations.SiftLoops
import SystemSettings
import Testing.LibraryOptimization

libName = "LargeTests"
level1Path = testPath ++ libName ++ ".lspc"
level1CPath = testPath ++ libName ++ ".c"

allLoopInterchangeTests =
  compileLibSpecToFileWithOptimization (sequenceOptimization "LoopInterchangeTestOpts"
                                                             [interchangeLoops,
                                                              siftLoops,
                                                              cleanupOperation])
                                                              level1Path level1CPath

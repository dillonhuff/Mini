module PartialLoopUnrollingTests(allPartialLoopUnrollingTests) where

import FullLoopUnrolling
import IndexExpressionOptimizations
import PartialLoopUnrolling
import SystemSettings
import Testing.LibraryOptimization

level1Path = projectPath ++ "Level1BLAS.lspc"
level1CPath = projectPath ++ "Level1BLAS.c"

allPartialLoopUnrollingTests = do
  compileLibSpecToFileWithOptimizations [partiallyUnrollAllLoopsBy 4, evalIExprConstants, fullyUnrollAllLoops] level1Path level1CPath

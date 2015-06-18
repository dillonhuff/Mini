module Optimizations.InnerLoopParallelizationTests(allInnerLoopParallelizationTests) where

import Data.List as L

import Core.IndexExpression
import Core.MiniOperation
import Core.MiniSyntax
import Optimizations.InnerLoopParallelization
import TestUtils.Dummies.Operation
import TestUtils.Module

allInnerLoopParallelizationTests = do
  testFunction (parLoopSize 4) innerLoopCases

parLoopSize factor op =
  let resultOp = tryParallelizeWithFixedUnrolling factor op
      stmts = blockStatements $ getOpBlock resultOp in
  L.length stmts

innerLoopCases =
  [(emptyOp, 0),
   (loadOp, 1),
   (vaddOp, 2),
   (vaddFixedSizeOp 8, 5)]

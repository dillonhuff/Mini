module Optimizations.InnerLoopParallelizationTests(allInnerLoopParallelizationTests) where

import Data.List as L

import Core.IndexExpression
import Core.MiniSyntax
import Optimizations.InnerLoopParallelization
import TestUtils

allInnerLoopParallelizationTests = do
  testFunction (parallelizeInnerLoops 2) innerLoopCases

innerLoopCases =
  L.map (\(x, y) -> (block x, block y))
  [([], []),
   ([unparallelizableLoop], [unparallelizableLoop]),
   ([addOneLoop], [parAddOneLoop])]

singleLoop stmts =
  for "i" (iConst 0) (iConst 1) (iVar "n") (block stmts) "l0"

unparallelizableLoop =
  singleLoop [load "x" "b" (iVar "i") "l1",
              plus "a" "x" "a" "l2"]

addOneLoop =
  singleLoop [load "x" "b" (iVar "i") "l1",
              loadConst "c" (floatLit 1.0) "l2",
              plus "y" "x" "c" "l3",
              store "b" (iVar "i") "y" "l4"]

parAddOneLoop =
  singleLoop [load "x_iter1" "b" (iVar "i") "l1_iter1",
              load "x_iter2" "b" (iAdd (iVar "i") (iConst 1)) "l1_iter2",
              loadConst "c_iter1" (floatLit 1.0) "l2_iter1",
              loadConst "c_iter2" (floatLit 1.0) "l2_iter2",
              plus "y_iter1" "x_iter1" "c_iter1" "l3_iter1",
              plus "y_iter2" "x_iter2" "c_iter2" "l3_iter2",
              store "b" (iVar "i") "x_iter1" "l4_iter1",
              store "b" (iAdd (iVar "i") (iConst 1)) "x_iter2" "l4_iter2"]

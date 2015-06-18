module Optimizations.FullLoopUnrollingTests(allFullLoopUnrollingTests) where

import Data.List as L

import Core.IndexExpression
import Optimizations.FullLoopUnrolling
import Core.MiniSyntax
import TestUtils.Module

allFullLoopUnrollingTests = do
  testFunction tryFullyUnrollLoop fullUnrollSuccessCases

fullUnrollSuccessCases =
  L.map (\(x, y) -> (x, y))
  [(for "i" (iConst 0) (iConst 1) (iConst 0) (block []) "", []),
   (for "j" (iConst 1) (iConst 2) (iConst 4) (block [load "a" "b" (iVar "j") ""]) "",
    [load "a" "b" (iConst 1) "",
     load "a" "b" (iConst 3) ""])]

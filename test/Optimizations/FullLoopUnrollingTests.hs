module Optimizations.FullLoopUnrollingTests(allFullLoopUnrollingTests) where

import Data.List as L

import Core.IndexExpression
import Optimizations.FullLoopUnrolling
import Core.MiniSyntax
import TestUtils.Dummies.Loop
import TestUtils.Dummies.Statement
import TestUtils.Module

allFullLoopUnrollingTests = do
  testFunction tryFullyUnrollLoop fullUnrollSuccessCases

fullUnrollSuccessCases =
  L.map (\(x, y) -> (x, y))
  [(pS0I1EC "i" 0 [], []),
   (pS0I1EC "i" 2 [rBA], [rBA, rBA, rBA]),
   (for "j" (iConst 1) (iConst 2) (iConst 4) (block [load "a" "b" (iVar "j") ""]) "",
    [load "a" "b" (iConst 1) "",
     load "a" "b" (iConst 3) ""])]

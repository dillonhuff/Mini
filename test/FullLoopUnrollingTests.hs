module FullLoopUnrollingTests(allFullyUnrollLoopTests) where

import Data.List as L

import FullLoopUnrolling
import IndexExpression
import Syntax
import TestUtils

allFullyUnrollLoopTests = do
  testFunction fullyUnrollLoop fullUnrollSuccessCases

fullUnrollSuccessCases =
  L.map (\(x, y) -> (x, Just y))
  [(for "i" (iConst 0) (iConst 1) (iConst 0) (block []) "", [])]

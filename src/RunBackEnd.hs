module RunBackEnd(runBackEnd) where

import Data.List as L
import Data.Map as M

import MatrixOperation
import MOpSyntax
import MiniOperation
import Syntax

runBackEnd :: [(MatrixOperation, [Map String Int])] -> Either String [Operation String] 
runBackEnd opsAndTestCases =
  let unOptimizedOpsAndTestCases = L.map (\(op, tests) -> (matrixOpToMiniOpNoOptimizations op, tests)) opsAndTestCases in
  Left "runBackEnd not implemented"

matrixOpToMiniOpNoOptimizations matOp =
  convertToMini $ matrixOperationToMOp matOp


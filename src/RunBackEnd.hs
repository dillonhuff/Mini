module RunBackEnd(runBackEnd) where

import Data.List as L
import Data.Map as M


import FullLoopUnrolling
import IndexExpressionOptimizations
import MatrixOperation
import MOpSyntax
import MiniOperation
import Syntax
import Testing.RuntimeEvaluation
import Testing.EvaluationResult

runBackEnd :: [(MatrixOperation, [Map String Int])] -> IO (Either String [Operation String])
runBackEnd opsAndTestCases =
  let ops = L.map fst opsAndTestCases
      testCasesList = L.map snd opsAndTestCases
      unOptimizedOps = L.map matrixOpToMiniOpNoOptimizations ops
      optimizedOps = L.map matrixOpToMiniOp ops
      operationNames = L.map getOpName unOptimizedOps in
  do
    scResults <- timeOperationsOnExamples "" operationNames testCasesList unOptimizedOps optimizedOps
    putStrLn $ show scResults
    case sanityCheckFailures scResults of
      [] -> return $ Right optimizedOps
      errors -> return $ Left $ L.concat $ L.intersperse "\n" $ L.map show errors

matrixOpToMiniOpNoOptimizations matOp =
  let mOp = matrixOperationToMOp matOp
      miniRes = convertToMini mOp in
  miniRes

matrixOpToMiniOp matOp =
  applyOptimization evalIExprConstants $
  applyOptimization fullyUnrollAllLoops $
  convertToMini $ matrixOperationToMOp matOp

sanityCheckFailures :: [Map (String, Map String Int) EvaluationResult] -> [(String, Map String Int)]
sanityCheckFailures runResults = L.map fst $ L.filter (\(_, res) -> not $ passedSanityCheck res) $ L.concatMap M.toList runResults

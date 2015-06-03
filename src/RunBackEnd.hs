module RunBackEnd(runBackEndWithOptimizations,
                  runBackEnd,
                  defaultOptimizations) where

import Data.List as L
import Data.Map as M

import FullLoopUnrolling
import IndexExpressionOptimizations
import LoopFusion
import MatrixOperation
import MOpSyntax
import MiniOperation
import Syntax
import TempBufferElimination
import Testing.RuntimeEvaluation
import Testing.EvaluationResult

runBackEndWithOptimizations :: [Optimization String] -> [(MatrixOperation, [Map String Int])] -> IO (Either String [Operation String])
runBackEndWithOptimizations optimizations opsAndTestCases =
  let ops = L.map fst opsAndTestCases
      testCasesList = L.map snd opsAndTestCases
      unOptimizedOps = L.map matrixOpToMiniOpNoOptimizations ops
      optimizedOps = L.map (matrixOpToMiniOpWithOptimizations optimizations) ops
      operationNames = L.map getOpName unOptimizedOps in
  do
    scResults <- timeOperationsOnExamples "" operationNames testCasesList unOptimizedOps optimizedOps
    putStrLn $ show scResults
    case sanityCheckFailures scResults of
      [] -> return $ Right optimizedOps
      errors -> return $ Left $ L.concat $ L.intersperse "\n" $ L.map show errors

runBackEnd opsAndTestCases =
  runBackEndWithOptimizations defaultOptimizations opsAndTestCases

matrixOpToMiniOpNoOptimizations matOp =
  let mOp = matrixOperationToMOp matOp
      miniRes = convertToMini mOp in
  miniRes

defaultOptimizations = [eliminateTempBuffers,
                        fuseAllTopLevelLoopsPossible,
                        evalIExprConstants,
                        fullyUnrollAllLoops]

matrixOpToMiniOpWithOptimizations [] matOp = convertToMini $ matrixOperationToMOp matOp
matrixOpToMiniOpWithOptimizations (opt:rest) matOp =
  applyOptimization opt $ matrixOpToMiniOpWithOptimizations rest matOp
  
sanityCheckFailures :: [Map (String, Map String Int) EvaluationResult] -> [(String, Map String Int)]
sanityCheckFailures runResults = L.map fst $ L.filter (\(_, res) -> not $ passedSanityCheck res) $ L.concatMap M.toList runResults

module BackEnd.RunBackEnd(runBackEndWithOptimization,
                          runBackEndNoChecksOrOptimizations,
                          runBackEnd,
                          defaultOptimization,
                          cleanupOperation,
                          cleanupFixedSizeOperation,
                          defaultFixedSizeOperation, defaultFixedSizeOperationUnrolling,
                          fuseAndParallelize,
                          nullOptimization) where

import Data.List as L
import Data.Map as M

import Core.MatrixOperation
import Core.MiniOperation
import Core.MiniSyntax
import Core.MOpSyntax
import Optimizations.ArrayCompaction
import Optimizations.CopyPropagation
import Optimizations.FullLoopUnrolling
import Optimizations.IndexExpressionOptimizations
import Optimizations.InnerLoopParallelization
import Optimizations.LoopFusion
import Optimizations.LoopInvariantCodeMotion
import Optimizations.RegisterSynonymDeletion
import Optimizations.SiftLoops
import Optimizations.TempBufferElimination
import Testing.EvaluationResult
import Testing.RuntimeEvaluation.Batch

runBackEndNoChecksOrOptimizations :: [(MatrixOperation, [Map String Int])] -> [Operation String]
runBackEndNoChecksOrOptimizations opsAndTestCases =
  L.map (\opAndTC -> matrixOpToMiniOpNoOptimizations $ fst opAndTC) opsAndTestCases
      
      
runBackEndWithOptimization :: Optimization String -> [(MatrixOperation, [Map String Int])] -> IO (Either String [Operation String])
runBackEndWithOptimization optimization opsAndTestCases =
  let ops = L.map fst opsAndTestCases
      testCasesList = L.map snd opsAndTestCases
      unOptimizedOps = L.map matrixOpToMiniOpNoOptimizations ops
      optimizedOps = L.map (applyOptimization optimization) unOptimizedOps
      operationNames = L.map getOpName unOptimizedOps in
  do
    scResults <- timeOperationsOnExamples "" operationNames testCasesList unOptimizedOps optimizedOps
    putStrLn $ show scResults
    case sanityCheckFailures scResults of
      [] -> return $ Right optimizedOps
      errors -> return $ Left $ L.concat $ L.intersperse "\n" $ L.map show errors

runBackEnd opsAndTestCases =
  runBackEndWithOptimization defaultOptimization opsAndTestCases

matrixOpToMiniOpNoOptimizations matOp =
  let mOp = matrixOperationToMOp matOp
      miniRes = convertToMini mOp in
  miniRes

nullOptimization =
  sequenceOptimization "Null" []
  
defaultOptimization =
  sequenceOptimization "DefaultOpts" [fuseAndParallelize, cleanupOperation]

cleanupOperation =
  sequenceOptimization "CleanupOpts" [evalIExprConstants,
                                      fullyUnrollAllLoops]

cleanupFixedSizeOperation =
  sequenceOptimization "CleanupFixedSize" [evalIExprConstants,
                                           fullyUnrollLoopsBelow 1]

defaultFixedSizeOperation =
  sequenceOptimization "FixedSizeDefault" [fuseAndParallelize,
                                           cleanupFixedSizeOperation]

defaultFixedSizeOperationUnrolling =
  sequenceOptimization "FixedSizeDefaultUnrolling" [fuseAndParallelizeUnrolling,
                                                    cleanupFixedSizeOperation]

fuseAndParallelize =
  sequenceOptimization "FuseAndParallelize" [moveConstantLoadsOutOfLoops,
                                             parallelizeInnerLoopsBy 4,
                                             deleteRegisterSynonyms,
                                             compactArrays,
                                             fuseAllTopLevelLoopsPossible,
                                             siftLoops,
                                             eliminateTempBuffers,
                                             compactArrays,
                                             fuseAllTopLevelLoopsPossible,
                                             propagateAllTopLevelCopiesPossible]

fuseAndParallelizeUnrolling =
  sequenceOptimization "FuseAndParallelizeUnrolling" [moveConstantLoadsOutOfLoops,
                                                      parallelizeInnerLoopsByWithFixedUnrolling 4,
                                                      deleteRegisterSynonyms,
                                                      compactArrays,
                                                      fuseAllTopLevelLoopsPossible,
                                                      siftLoops,
                                                      eliminateTempBuffers,
                                                      compactArrays,
                                                      fuseAllTopLevelLoopsPossible,
                                                      propagateAllTopLevelCopiesPossible]

sanityCheckFailures :: [Map (String, Map String Int) EvaluationResult] -> [(String, Map String Int)]
sanityCheckFailures runResults = L.map fst $ L.filter (\(_, res) -> not $ passedSanityCheck res) $ L.concatMap M.toList runResults

module Testing.RuntimeEvaluation.FixedSizes(timeImplementationsFixedSizes,
                                            timeOperationWithOptimizations) where

import Control.Monad
import Data.List as L
import Data.Map as M
import System.IO.Strict as S
import System.Process

import Core.MiniOperation
import Core.MiniSyntax
import SystemSettings
import Testing.EvaluationResult
import Testing.RuntimeEvaluation.Basic
import Testing.TestHarness

type OperationName = String
type OptimizationName = String

timeOperationsWithOptimizations :: (Ord a, Show a) =>
                                             a ->
                                             String ->
                                             [Operation a] ->
                                             [Optimization a] ->
                                             IO (Map OperationName (Map OptimizationName EvaluationResult))
timeOperationsWithOptimizations dummyAnn testName operations optimizations = do
  results <- sequence $ L.map (\op -> timeOperationWithOptimizations dummyAnn testName op optimizations) operations
  return $ M.fromList $ L.zip (L.map getOpName operations) results

timeOperationWithOptimizations :: (Ord a, Show a) =>
                                  a ->
                                  String ->
                                  Operation a ->
                                  [Optimization a] ->
                                  IO (Map OptimizationName EvaluationResult)
timeOperationWithOptimizations dummyAnn testName operation optimizations = do
  results <- sequence $ L.map (\opt -> timeOperationWithOptimization dummyAnn testName M.empty operation opt) optimizations
  return $ M.fromList results
    

timeImplementationsFixedSizes dummyAnn opName sanityCheckImpl impls =
  timeImplementations M.empty dummyAnn opName sanityCheckImpl impls

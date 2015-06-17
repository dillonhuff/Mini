module Testing.RuntimeEvaluation.FixedSizes(timeImplementationsFixedSizes) where

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

timeOperationsWithOptimizationsFixedSizes :: (Ord a, Show a) =>
                                             a ->
                                             String ->
                                             [Operation a] ->
                                             [Optimization a] ->
                                             IO (Map OperationName (Map OptimizationName EvaluationResult))
timeOperationsWithOptimizationsFixedSizes dummyAnn testName operations optimizations =
{-  let operationNames = L.map getOpName operations in
  do
    results <- sequence $ L.map (\impl -> timeOptimizationsByName dummyAnn testName impl optimizations) operations-}
    error "timeOperationsWithOptimizationsFixedSizes"
--  sequence $ L.map (\impl -> timeOptimizations dummyAnn testName impl optimizations) opImpls

{-timeOptimizationsByName :: (Ord a, Show a) => a -> String -> Operation a -> [Optimization a] -> IO (Map OptimizationName EvaluationResult)
timeOptimizationsByName dummyAnn opName impl optimizations =
  let optimizedImpls = L.map (\opt -> applyOptimization opt impl) optimizations in
  timeImplementationsByName M.empty dummyAnn opName (Just impl) optimizedImpls-}

timeImplementationsFixedSizes dummyAnn opName sanityCheckImpl impls =
  timeImplementations M.empty dummyAnn opName sanityCheckImpl impls

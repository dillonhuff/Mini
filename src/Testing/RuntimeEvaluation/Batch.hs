module Testing.RuntimeEvaluation.Batch(timeOperationsOnExamples) where

import Control.Monad
import Data.List as L
import Data.Map as M

import Core.MiniOperation
import Core.MiniSyntax
import SystemSettings
import Testing.EvaluationResult
import Testing.RuntimeEvaluation.Basic
import Testing.TestHarness

timeOperationsOnExamples :: (Ord a, Show a) => a -> [String] -> [[Map String Int]] -> [Operation a] -> [Operation a] -> IO [Map (String, Map String Int) EvaluationResult]
timeOperationsOnExamples dummyAnn opNames examplesList scImps impls =
  sequence $ L.zipWith4 (\opName examples scImp impl -> timeOperationOnExamples dummyAnn opName examples scImp impl)
                        opNames
                        examplesList
                        scImps
                        impls

timeOperationOnExamples dummyAnn opName examples scImp impl =
  liftM M.fromList $ 
  sequence $ L.map (\ex -> timeOperationOnExample dummyAnn opName ex scImp impl) examples

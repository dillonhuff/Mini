module RuntimeEvaluation(timeImplementations,
                         timeImplementationsFixedSizes,
                         timeOperationsWithOptimizationsFixedSizes) where

import Control.Monad
import Data.List as L
import Data.Map as M
import System.IO.Strict as S
import System.Process

import EvaluationResult
import MiniOperation
import Syntax
import SystemSettings
import TestHarness

timeOperationsWithOptimizationsFixedSizes :: (Ord a, Show a) => a -> String -> [Operation a] -> [Optimization a] -> IO [Map (Operation a) EvaluationResult]
timeOperationsWithOptimizationsFixedSizes dummyAnn testName opImpls optimizations =
  sequence $ L.map (\impl -> timeOptimizations dummyAnn testName impl optimizations) opImpls

timeOptimizations :: (Ord a, Show a) => a -> String -> Operation a -> [Optimization a] -> IO (Map (Operation a) EvaluationResult)
timeOptimizations dummyAnn opName originalImpl opts =
  let optimizedImpls = L.map (\opt -> applyOptimization opt originalImpl) opts in
  timeImplementationsFixedSizes dummyAnn opName (Just originalImpl) optimizedImpls

timeImplementationsFixedSizes dummyAnn opName sanityCheckImpl impls =
  timeImplementations M.empty dummyAnn opName sanityCheckImpl impls

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

timeOperationOnExample dummyAnn opName example scImp impl = do
  timeRes <- timeImplementations example dummyAnn opName (Just scImp) [impl]
  let evalRes = snd $ L.head $ M.toList $ timeRes in
    return ((opName, example), evalRes)
  
timeImplementations :: (Ord a, Show a) => Map String Int -> a -> String -> Maybe (Operation a) -> [Operation a] -> IO (Map (Operation a) EvaluationResult)
timeImplementations indexVals dummyAnn opName sanityCheckImpl impls =
  let testCode = cTestHarness dummyAnn indexVals opName sanityCheckImpl impls in
  do
    runCTestCode opName testCode
    opNameToEvalResultMap <- readResultFile opName
    return $ reconstructOpMap impls opNameToEvalResultMap

runCTestCode :: String -> String -> IO ()
runCTestCode opName testStr = do
  writeFile (cFileName opName) testStr
  runCommandStrict $ compileString opName
  runCommandStrict $ runString opName
  return ()

runCommandStrict str = do
  putStrLn $ str
  cmdHandle <- runCommand str
  waitForProcess cmdHandle
  return ()

readResultFile :: String -> IO (Map String EvaluationResult)
readResultFile opName = do
  timingResults <- S.readFile (dataFileName opName)
  return $ parseTimingResults timingResults

reconstructOpMap :: (Show a, Ord a) => [Operation a] -> Map String EvaluationResult -> Map (Operation a) EvaluationResult
reconstructOpMap impls nameTimingResultMap =
  M.fromList $ L.map (\imp -> (imp, lookupOpTimeResByName nameTimingResultMap imp)) impls

lookupOpTimeResByName :: (Show a, Ord a) => Map String EvaluationResult -> Operation a -> EvaluationResult
lookupOpTimeResByName m op =
  case M.lookup (getOpName op) m of
    Just timeRes -> timeRes
    Nothing -> error $ "could not find result " ++ show op ++ " in " ++ show m


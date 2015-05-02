module RuntimeEvaluation(timeImplementations,
                         timeOperationsWithOptimizations) where

import Data.List as L
import Data.Map as M
import System.IO.Strict as S
import System.Process

import MiniOperation
import Syntax
import SystemSettings
import TestHarness

timeOperationsWithOptimizations :: (Ord a, Show a) => a -> String -> [Operation a] -> [Optimization a] -> IO [Map (Operation a) EvaluationResult]
timeOperationsWithOptimizations dummyAnn testName opImpls optimizations =
  sequence $ L.map (\impl -> timeOptimizations dummyAnn testName impl optimizations) opImpls

timeOptimizations :: (Ord a, Show a) => a -> String -> Operation a -> [Optimization a] -> IO (Map (Operation a) EvaluationResult)
timeOptimizations dummyAnn opName originalImpl opts =
  let optimizedImpls = L.map (\opt -> applyOptimization opt originalImpl) opts in
  timeImplementations dummyAnn opName (Just originalImpl) optimizedImpls

timeImplementations :: (Ord a, Show a) => a -> String -> Maybe (Operation a) -> [Operation a] -> IO (Map (Operation a) EvaluationResult)
timeImplementations dummyAnn opName sanityCheckImpl impls =
  let testCode = cTestHarness dummyAnn opName sanityCheckImpl impls in
  do
    runCTestCode opName testCode
    opNameToEvalResultMap <- readResultFile opName
    return $ reconstructOpMap impls opNameToEvalResultMap

runCTestCode :: String -> String -> IO ()
runCTestCode opName testStr = do
  writeFile (cFileName opName) testStr
  runCommandStrict $ compileString opName
  runCommandStrict $ runString opName
{-  putStrLn $ "Compile string: " ++ compileString opName
  compCommand <- runCommand $ compileString opName
  waitForProcess compCommand
  putStrLn $ "Run string: " ++ runString opName
  execCommand <- runCommand $ runString opName
  waitForProcess execCommand-}
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


module RuntimeEvaluation(timeImplementations) where

import Data.List as L
import Data.Map as M
import System.Process

import Syntax
import SystemSettings
import TestHarness

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
  putStrLn $ "Compile string: " ++ compileString opName
  runCommand $ compileString opName
  putStrLn $ "Run string: " ++ runString opName
  runCommand $ runString opName
  return ()

readResultFile :: String -> IO (Map String EvaluationResult)
readResultFile opName = do
  timingResults <- readFile (dataFileName opName)
  return $ parseTimingResults timingResults

reconstructOpMap :: (Show a, Ord a) => [Operation a] -> Map String EvaluationResult -> Map (Operation a) EvaluationResult
reconstructOpMap impls nameTimingResultMap =
  M.fromList $ L.map (\imp -> (imp, lookupOpTimeResByName nameTimingResultMap imp)) impls

lookupOpTimeResByName :: (Show a, Ord a) => Map String EvaluationResult -> Operation a -> EvaluationResult
lookupOpTimeResByName m op =
  case M.lookup (getOpName op) m of
    Just timeRes -> timeRes
    Nothing -> error $ "could not find result " ++ show op ++ " in " ++ show m


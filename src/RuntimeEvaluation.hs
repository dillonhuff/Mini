module RuntimeEvaluation(timeImplementations) where

import Data.List as L
import Data.Map as M
import System.Process

import Syntax
import SystemSettings
import TestHarness

timeImplementations :: (Ord a, Show a) => String -> Maybe (Operation a) -> [Operation a] -> IO (Map (Operation a) EvaluationResult)
timeImplementations fileName sanityCheckImpl impls =
  let testCode = cTestHarness (evalPath ++ fileName) sanityCheckImpl impls in
  do
    resultFileName <- runCTestCode fileName testCode
    opNameToEvalResultMap <- readResultFile resultFileName
    putStrLn $ "Delete string: " ++ "rm -rf " ++ resultFileName
    res <- runCommand $ "rm -rf " ++ resultFileName
    return $ reconstructOpMap impls opNameToEvalResultMap

runCTestCode :: String -> String -> IO String
runCTestCode fileName testStr =
  let filePath = evalPath ++ fileName in
  do
    writeFile filePath testStr
    putStrLn $ "Compile string: " ++ compileString filePath
    runCommand $ compileString filePath
    putStrLn $ "Run string: " ++ runString filePath
    runCommand $ runString filePath
    return $ dataFileName filePath

readResultFile :: String -> IO (Map String EvaluationResult)
readResultFile fileName = do
  timingResults <- readFile fileName
  return $ parseTimingResults timingResults

reconstructOpMap :: (Show a, Ord a) => [Operation a] -> Map String EvaluationResult -> Map (Operation a) EvaluationResult
reconstructOpMap impls nameTimingResultMap =
  M.fromList $ L.map (\imp -> (imp, lookupOpTimeResByName nameTimingResultMap imp)) impls

lookupOpTimeResByName :: (Show a, Ord a) => Map String EvaluationResult -> Operation a -> EvaluationResult
lookupOpTimeResByName m op =
  case M.lookup (getOpName op) m of
    Just timeRes -> timeRes
    Nothing -> error $ "could not find result " ++ show op ++ " in " ++ show m


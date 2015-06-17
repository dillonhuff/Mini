module Testing.RuntimeEvaluation.Basic(timeOperationWithOptimization,
                                       timeOperationOnExample,
                                       timeImplementations) where

import Data.List as L
import Data.Map as M
import System.IO.Strict as S
import System.Process

import Core.MiniOperation
import Core.MiniSyntax
import SystemSettings
import Testing.EvaluationResult
import Testing.TestHarness

timeOperationWithOptimization dummyAnn opName example operation optimization =
  let optimizedOperation = applyOptimization optimization operation in
  do
    ((_, _), evalRes) <- timeOperationOnExample dummyAnn opName example operation optimizedOperation
    return (optimizationName optimization, evalRes)

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
  putStrLn $ "reading result file " ++ opName
  timingResults <- S.readFile (dataFileName opName)
  putStrLn $ show timingResults
  return $ parseTimingResults timingResults

reconstructOpMap :: (Show a, Ord a) => [Operation a] -> Map String EvaluationResult -> Map (Operation a) EvaluationResult
reconstructOpMap impls nameTimingResultMap =
  M.fromList $ L.map (\imp -> (imp, lookupOpTimeResByName nameTimingResultMap imp)) impls

lookupOpTimeResByName :: (Show a, Ord a) => Map String EvaluationResult -> Operation a -> EvaluationResult
lookupOpTimeResByName m op =
  case M.lookup (getOpName op) m of
    Just timeRes -> timeRes
    Nothing -> error $ "could not find result " ++ show op ++ " in " ++ show m



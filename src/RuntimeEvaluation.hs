module RuntimeEvaluation(timeImplementations) where

import Data.Map as M

import Syntax

data EvaluationResult
  = EvaluationResult [Int] Bool
    deriving (Eq, Ord, Show)

timeImplementations :: (Show a) => String -> Maybe (Operation a) -> [Operation a] -> IO (Map (Operation a) EvaluationResult)
timeImplementations fileName sanityCheckImpl impls =
  let testCode = cTestHarness sanityCheckImpl impls in
  do
    resultFileName <- runCTestCode fileName testCode
    opNameToEvalResultMap <- readResultFile resultFileName
    return $ reconstructOpMap impls opNameToEvalResultMap

cTestHarness :: (Show a) => Maybe (Operation a) -> [Operation a] -> String
cTestHarness scImp implsToTime =
  "int main() {\n\treturn 0;\n}\n"

runCTestCode :: String -> String -> IO String
runCTestCode fileName testStr = do
  writeFile fileName testStr
  return fileName

readResultFile :: String -> IO (Map String EvaluationResult)
readResultFile fileName = do
  timingResults <- readFile fileName
  return $ parseTimingResults timingResults

reconstructOpMap :: [Operation a] -> Map String EvaluationResult -> Map (Operation a) EvaluationResult
reconstructOpMap impls nameTimingResultMap = error "reconstructOpMap not implemented"

parseTimingResults :: String -> Map String EvaluationResult
parseTimingResults str = M.empty

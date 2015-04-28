module TestHarness(cTestHarness,
                   parseTimingResults,
                   EvaluationResult) where

import Data.List as L
import Data.Map as M

import Syntax
import SystemSettings

data EvaluationResult
  = EvaluationResult [Int] Bool
    deriving (Eq, Ord, Show)

cTestHarness :: (Show a) => String -> Maybe (Operation a) -> [Operation a] -> String
cTestHarness fileName (Just scImp) implsToTime =
  let opNames = L.map getOpName implsToTime
      args = getOpArguments scImp in
  includeStr ++
  sanityCheckFunc (getOpName scImp) opNames args ++
  timingFunc opNames args ++
  "int main() {\n\t" ++
  createDataFileString fileName ++
  "sanity_check_impls(fp);\n\ttime_impls(fp);\n" ++
  closeDataFileString fileName ++
  "return 0;\n}\n"
cTestHarness _ _ _ = error "Are you sure you don't want to do a sanity check?"

parseTimingResults :: String -> Map String EvaluationResult
parseTimingResults str = M.empty

sanityCheckFunc :: String -> [String] -> [(String, Type)] -> String
sanityCheckFunc scFuncName implFuncNames argBuffers =
  "void sanity_check_impls(FILE* df) {\n\n}\n";

timingFunc :: [String] -> [(String, Type)] -> String
timingFunc implFuncNames argBuffers = "void time_impls(FILE* df) {\n\n}\n";

createDataFileString fileName = "\tFILE* fp = fopen(\"" ++ dataFileName fileName ++ "\", \"w\");\n"
closeDataFileString fileName = "\tfclose(fp);\n"

includeStr = "#include <stdio.h>\n"

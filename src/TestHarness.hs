module TestHarness(cTestHarness,
                   parseTimingResults,
                   EvaluationResult) where

import Data.List as L
import Data.Map as M

import CGen
import Syntax
import SystemSettings

data EvaluationResult
  = EvaluationResult [Int] Bool
    deriving (Eq, Ord, Show)

evaluationResult = EvaluationResult

cTestHarness :: (Show a) => String -> Maybe (Operation a) -> [Operation a] -> String
cTestHarness fileName (Just scImp) implsToTime = 
  let opNames = L.map getOpName implsToTime
      args = getOpArguments scImp
      cImplFuncs = L.map toCFunc implsToTime
      cSCFunc = toCFunc scImp
      implFuncCode = L.concatMap (\cF -> prettyPrint 0 cF) cImplFuncs
      scFuncCode = prettyPrint 0 cSCFunc in
  includeStr ++
  implFuncCode ++
  scFuncCode ++
  sanityCheckFunc (getOpName scImp) opNames args ++
  timingFunc opNames args ++
  "int main() {\n" ++
  createDataFileString fileName ++
  "\tsanity_check_impls(fp);\n\ttime_impls(fp);\n" ++
  closeDataFileString fileName ++
  "\treturn 0;\n}\n"
cTestHarness _ _ _ = error "Are you sure you don't want to do a sanity check?"

parseTimingResults :: String -> Map String EvaluationResult
parseTimingResults str =
  let strLines = L.lines str
      scLines = L.takeWhile (\l -> l /= scTimingSeparator) strLines in
  M.fromList $ parseSCResults scLines

scTimingSeparator = "#"

parseSCResults :: [String] -> [(String, EvaluationResult)]
parseSCResults [] = []
parseSCResults (n:passFail:rest) = (n, evaluationResult [] $ if passFail == "passed" then True else False):(parseSCResults rest)
parseSCResults other = error $ "parseSCResults failed with " ++ show other

sanityCheckFunc :: String -> [String] -> [(String, Type)] -> String
sanityCheckFunc scFuncName implFuncNames argBuffers =
  "void sanity_check_impls(FILE* df) {\n\n}\n";

timingFunc :: [String] -> [(String, Type)] -> String
timingFunc implFuncNames argBuffers = "void time_impls(FILE* df) {\n\n}\n";

createDataFileString fileName = "\tFILE* fp = fopen(\"" ++ dataFileName fileName ++ "\", \"w\");\n"
closeDataFileString fileName = "\tfclose(fp);\n"

includeStr = "#include <stdio.h>\n"

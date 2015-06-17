module Testing.BenchmarkReporting() where

import Data.List as L
import Data.Map as M

import BackEnd.RunBackEnd
import Core.MiniOperation
import FrontEnd.RunFrontEnd
import Reporting.Plot
import Testing.EvaluationResult
import Testing.RuntimeEvaluation.FixedSizes

fixedSizeLibBenchmarkReport :: [Optimization String] ->
                               String ->
                               String ->
                               IO ()
fixedSizeLibBenchmarkReport opts libPath reportPath = do
  fileContents <- readFile libPath
  res <- runFrontEnd libPath fileContents
  case res of
    Left err -> putStrLn err
    Right opsAndTestCases ->
      let ops = runBackEndNoChecksOrOptimizations opsAndTestCases in
      generateBenchmarkReport opts ops reportPath

generateBenchmarkReport opts ops reportPath = do
  runRes <- timeOperationsWithOptimizations "" "test" ops opts
  let listRes = L.map (\(opN, m) -> (opN, M.toList m)) $ M.toList runRes
      sortedRes = sortByOptimizationName listRes
      values = L.map (\(opN, optsToResults) -> (opN, L.map (\(opt, res) -> avgCyclesPerRun res) optsToResults)) sortedRes
      titles = L.map fst $ snd $ L.head sortedRes in
    simpleBar "testChart2.png" reportPath titles values

sortByOptimizationName :: [(String, [(String, EvaluationResult)])] ->
                          [(String, [(String, EvaluationResult)])]
sortByOptimizationName listRes =
  L.map (\(opN, optsToResults) -> (opN, L.sortBy (\(on1, _) (on2, _) -> compare on1 on2) optsToResults)) listRes

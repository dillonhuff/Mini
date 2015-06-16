module Testing.BenchmarkReporting() where

import Data.List as L
import Data.Map as M

import BackEnd.RunBackEnd
import Core.MiniOperation
import FrontEnd.RunFrontEnd
import Reporting.Plot
import Testing.EvaluationResult
import Testing.RuntimeEvaluation

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
  runRes <- timeOperationsWithOptimizationsFixedSizes "" "test" ops opts
  let titles = L.map optimizationName opts
      valNames = L.map getOpName ops
      avgCycles = L.map (\evalMap -> L.map avgCyclesPerRun $ L.map snd $ M.toList evalMap) runRes
      values = L.zip valNames avgCycles in
    do
      putStrLn $ show runRes
      simpleBar "testChart.png" titles values
      

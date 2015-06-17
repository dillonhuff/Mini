module Testing.BenchmarkReporting() where

import Data.Either
import Data.List as L
import Data.Map as M

import BackEnd.RunBackEnd
import Core.MiniOperation
import FrontEnd.RunFrontEnd
import Reporting.Report
import Testing.EvaluationResult
import Testing.RuntimeEvaluation.FixedSizes

fixedSizeLibsBenchmarkReport :: [Optimization String] ->
                               [String] ->
                               String ->
                               IO ()
fixedSizeLibsBenchmarkReport opts libPaths reportPath = do
  reports <- sequence $ L.map (\libPath -> fixedSizeLibBenchmarkReport opts libPath) libPaths
  let reps = rights reports
      allReps = L.foldl concatReports (report "perfrep" []) reps in
    writeReportHtml reportPath allReps

genFixedSizeLibBenchmarkReport :: [Optimization String] ->
                                  String ->
                                  String ->
                                  IO ()
genFixedSizeLibBenchmarkReport opts libPath reportPath = do
  rep <- fixedSizeLibBenchmarkReport opts libPath
  case rep of
    Left err -> putStrLn err
    Right rprt -> writeReportHtml reportPath rprt

fixedSizeLibBenchmarkReport opts libPath = do
  fileContents <- readFile libPath
  res <- runFrontEnd libPath fileContents
  case res of
    Left err -> return $ Left err
    Right opsAndTestCases ->
      let ops = runBackEndNoChecksOrOptimizations opsAndTestCases in
      do
        r <- genReport opts ops
        return $ Right r

genReport opts ops = do
  runRes <- timeOperationsWithOptimizations "" "test" ops opts
  let perfChart = performanceChart runRes
      scResults = sanityCheckResults $ L.map (\(opN, m) -> (opN, M.toList m)) $ M.toList runRes in
    return $ report "test_lib_report" [perfChart, scResults]

performanceChart runRes =
  let listRes = L.map (\(opN, m) -> (opN, M.toList m)) $ M.toList runRes
      sortedRes = sortByOptimizationName listRes
      values = L.map (\(opN, optsToResults) -> (opN, L.map (\(opt, res) -> avgCyclesPerRun res) optsToResults)) sortedRes
      titles = L.map fst $ snd $ L.head sortedRes in
  dblBarPlotComp "Performance Comparison" titles values

sanityCheckResults runRes =
  let scResults = L.concatMap sanityCheckResultStringForOperation runRes in
  strListComp "Sanity Check Results" scResults

sanityCheckResultStringForOperation (opName, optsAndResults) =
  L.map (\(opt, result) -> opName ++ " with " ++ opt ++ ": " ++ (show $ passedSanityCheck result)) optsAndResults

sortByOptimizationName :: [(String, [(String, EvaluationResult)])] ->
                          [(String, [(String, EvaluationResult)])]
sortByOptimizationName listRes =
  L.map (\(opN, optsToResults) -> (opN, L.sortBy (\(on1, _) (on2, _) -> compare on1 on2) optsToResults)) listRes

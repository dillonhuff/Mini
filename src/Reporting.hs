module Reporting(hfSanityCheckReport) where

import Data.List as L
import Data.Map as M

import EvaluationResult
import MiniOperation

hfSanityCheckReport :: String -> Map (Operation a) EvaluationResult -> String
hfSanityCheckReport opName opResMap =
  title opName ++
  (L.concatMap hfOpSCReport $ M.toList opResMap) ++
  footer

title t = "\n============================= " ++ t ++ " ====================================\n"
sectionTitle t = "-------------------------- " ++ t ++ " --------------------------------------\n"
footer = "\n==============================================================================\n"

hfOpSCReport :: (Operation a, EvaluationResult) -> String
hfOpSCReport (op, res) =
  sectionTitle (getOpName op) ++
  hfSanityCheckRes res ++
  hfOptimizationsAppliedList op ++
  hfAvgCyclesPerRun res

hfSanityCheckRes res =
  case passedSanityCheck res of
    True -> "Sanity check passed\n"
    False -> "SANITY CHECK FAILED\n"

hfOptimizationsAppliedList op =
  "Optimizations applied:\n" ++
  (L.concatMap (\n -> "\t" ++ show n)  $ getOptimizationsApplied op) ++
  "\n"

hfAvgCyclesPerRun op =
  "Avg. cycles per run: " ++ (show $ avgCyclesPerRun op)

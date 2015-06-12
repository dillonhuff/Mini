module Optimizations.InnerLoopParallelization(parallelizeInnerLoopsBy) where

import Data.List as L
import Data.Map as M

import Analysis.Dependence.Graph
import Analysis.Dependence.RegisterReduction
import Analysis.Loop
import Core.IndexExpression
import Core.LoopTransformations
import Core.MiniOperation
import Core.MiniSyntax
import Optimizations.PartialLoopUnrolling
import Utils.MapUtils

parallelizeInnerLoopsBy :: Int -> Optimization String
parallelizeInnerLoopsBy parFactor =
  optimization
        "ParallelizeInnerLoops"
        (\op -> applyToOpBlock (parallelizeInnerLoops (getMiniOpSymtab op) parFactor) op)

parallelizeInnerLoops symtab parFactor b =
  expandBlockStatements (tryToParallelizeInnerLoop symtab (registerUsageInfo b) parFactor) b

tryToParallelizeInnerLoop symtab regUseInfo parFactor stmt =
  case isMapLoop regUseInfo stmt of
    True -> partiallyUnrollAndIntersperse symtab parFactor stmt
    False -> [stmt]

isMapLoop usageInfo stmt =
  isInnerMapLoop usageInfo stmt && forInc stmt == (iConst 1)

isInnerMapLoop usageInfo stmt =
  isFor stmt &&
  noDeeperLoops stmt &&
  allRegistersWrittenAreOnlyUsedInLoop usageInfo stmt &&
  noLoopCarriedFlowDependenciesInInnerLoop stmt

registerUsageInfo b =
  let stmts = L.concatMap nonLoopStatements $ blockStatements b
      regLabelList = L.concatMap (\stmt -> L.map (\op -> (op, label stmt)) $ L.filter (\op -> not $ isBufferVal op) $ allOperands stmt) stmts in
  L.foldl addRegToUseMap M.empty regLabelList

addRegToUseMap m (reg, lab) =
  case M.lookup reg m of
    Just vals -> M.insert reg (lab:vals) m
    Nothing -> M.insert reg [lab] m

allRegistersWrittenAreOnlyUsedInLoop usageInfo stmt =
  let bodyStmts = nonLoopStatements stmt
      bodyStmtLabels = L.map label bodyStmts
      registersWritten = L.filter (\op -> not $ isBufferVal op) $ L.map operandWritten bodyStmts in
  L.and $ L.map (\reg -> noUsageOutside usageInfo bodyStmtLabels reg) registersWritten

noUsageOutside usageInfo labels reg =
  let allUseLabels = lookupF reg usageInfo in
  L.and $ L.map (\l -> L.elem l labels) allUseLabels


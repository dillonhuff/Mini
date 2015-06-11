module Optimizations.InnerLoopParallelization(parallelizeInnerLoops) where

import Data.List as L
import Data.Map as M

import Analysis.Dependence.RegisterReduction
import Core.IndexExpression
import Core.MiniSyntax
import Utils.MapUtils

parallelizeInnerLoops parFactor b =
  transformBlock (tryToParallelizeInnerLoop (registerUsageInfo b) parFactor) b

tryToParallelizeInnerLoop regUseInfo parFactor stmt =
  case isMapLoop regUseInfo stmt of
    True -> stmt
    False -> stmt

isMapLoop usageInfo stmt =
  isInnerMapLoop usageInfo stmt && forInc stmt == (iConst 1)

isInnerMapLoop usageInfo stmt =
  isFor stmt &&
  noDeeperLoops stmt &&
  allRegistersWrittenAreOnlyUsedInLoop usageInfo stmt &&
  noLoopCarriedFlowDependencies stmt

registerUsageInfo b =
  let stmts = L.concatMap nonLoopStatements $ blockStatements b
      regLabelList = L.concatMap (\stmt -> L.map (\op -> (op, label stmt)) $ L.filter (\op -> not $ isBufferVal op) $ allOperands stmt) stmts in
  L.foldl addRegToUseMap M.empty regLabelList

addRegToUseMap m (reg, lab) =
  case M.lookup reg m of
    Just vals -> M.insert reg (lab:vals) m
    Nothing -> M.insert reg [lab] m

noDeeperLoops stmt =
  let nonLoopStmtsInBody = nonLoopStatements stmt
      stmtsInBody = blockStatements $ forBody stmt in
  L.length (L.intersect nonLoopStmtsInBody stmtsInBody) == L.length stmtsInBody

allRegistersWrittenAreOnlyUsedInLoop usageInfo stmt =
  let bodyStmts = nonLoopStatements stmt
      bodyStmtLabels = L.map label bodyStmts
      registersWritten = L.filter (\op -> not $ isBufferVal op) $ L.map operandWritten bodyStmts in
  L.and $ L.map (\reg -> noUsageOutside usageInfo bodyStmtLabels reg) registersWritten

noUsageOutside usageInfo labels reg =
  let allUseLabels = lookupF reg usageInfo in
  L.and $ L.map (\l -> L.elem l labels) allUseLabels

noLoopCarriedFlowDependencies stmt =
  case buildLoopDependenceGraph stmt of
    Just g -> True
    Nothing -> False

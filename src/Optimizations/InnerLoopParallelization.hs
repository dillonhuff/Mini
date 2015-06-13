module Optimizations.InnerLoopParallelization(parallelizeInnerLoopsBy) where

import Control.Monad.State.Lazy
import Data.List as L
import Data.Map as M

import Analysis.Basic
import Analysis.Dependence.Graph
import Analysis.Dependence.RegisterReduction
import Analysis.Loop
import Core.IndexExpression
import Core.LoopTransformations
import Core.MiniOperation
import Core.MiniSyntax
import Core.Operand
import Optimizations.PartialLoopUnrolling
import Utils.MapUtils

parallelizeInnerLoopsBy :: Int -> Optimization String
parallelizeInnerLoopsBy parFactor =
  optimization
        "ParallelizeInnerLoops"
        (tryParallelize parFactor)

tryParallelize parFactor op =
  let st = getMiniOpSymtab op
      b = getOpBlock op
      opts = getOptimizationsApplied op
      (newOpBlock, newSt) = (runState $ parallelizeInnerLoops parFactor b) st in
  makeOperation (getOpName op) opts newSt newOpBlock

parallelizeInnerLoops parFactor b =
  expandBlockStatementsM (tryToParallelizeInnerLoop (registerUsageLocations b) parFactor) b

tryToParallelizeInnerLoop :: Map Operand [String] -> Int -> Statement String -> State MiniSymtab [Statement String]
tryToParallelizeInnerLoop regUseInfo parFactor stmt =
  case isMapLoop regUseInfo stmt of
    True -> do
      symtab <- get
      let (newSymtab, newStmts) = partiallyUnrollAndIntersperse symtab parFactor stmt in
        do
          put newSymtab
          return newStmts
    False -> return [stmt]

isMapLoop usageInfo stmt =
  isInnerMapLoop usageInfo stmt && forInc stmt == (iConst 1)

isInnerMapLoop usageInfo stmt =
  isFor stmt &&
  noDeeperLoops stmt &&
  allRegistersWrittenAreOnlyUsedInLoop usageInfo stmt &&
  noLoopCarriedFlowDependenciesInInnerLoop stmt

allRegistersWrittenAreOnlyUsedInLoop usageInfo stmt =
  let bodyStmts = nonLoopStatements stmt
      bodyStmtLabels = L.map label bodyStmts
      registersWritten = L.filter (\op -> not $ isBufferVal op) $ L.map operandWritten bodyStmts in
  L.and $ L.map (\reg -> noUsageOutside usageInfo bodyStmtLabels reg) registersWritten

noUsageOutside usageInfo labels reg =
  let allUseLabels = lookupF reg usageInfo in
  L.and $ L.map (\l -> L.elem l labels) allUseLabels


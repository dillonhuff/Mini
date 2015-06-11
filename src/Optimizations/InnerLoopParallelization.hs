module Optimizations.InnerLoopParallelization(parallelizeInnerLoops) where

import Data.List as L

import Analysis.Dependence.RegisterReduction
import Core.IndexExpression
import Core.MiniSyntax

parallelizeInnerLoops parFactor b =
  transformBlock (tryToParallelizeInnerLoop (registerUsageInfo b) parFactor) b

tryToParallelizeInnerLoop regUseInfo parFactor stmt =
  case isMapLoop regUseInfo stmt of
    True -> error "tryToParallelizeInnerLoop"
    False -> stmt

isMapLoop usageInfo stmt =
  isInnerMapLoop usageInfo stmt && forInc stmt == (iConst 1)

isInnerMapLoop usageInfo stmt =
  isFor stmt &&
  noDeeperLoops stmt &&
  allRegistersWrittenAreOnlyUsedInLoop usageInfo stmt &&
  noLoopCarriedFlowDependencies stmt

registerUsageInfo b =
  error "registerUsageInfo"

noDeeperLoops stmt =
  let nonLoopStmtsInBody = nonLoopStatements stmt
      stmtsInBody = blockStatements $ forBody stmt in
  L.length (L.intersect nonLoopStmtsInBody stmtsInBody) == L.length stmtsInBody

allRegistersWrittenAreOnlyUsedInLoop usageInfo stmt =
  True

noLoopCarriedFlowDependencies stmt =
  case buildLoopDependenceGraph stmt of
    Just g -> error "loop graph"
    Nothing -> False

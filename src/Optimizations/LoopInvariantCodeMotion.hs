module Optimizations.LoopInvariantCodeMotion(moveConstantLoadsOutOfLoops,
                                             pullConstantLoadsOutOfLoops) where

import Data.List as L
import Data.Map as M

import Analysis.Basic
import Core.IndexExpression
import Core.MiniOperation
import Core.MiniSyntax

moveConstantLoadsOutOfLoops :: (Show a) => Optimization a
moveConstantLoadsOutOfLoops =
  optimization
        "MoveConstantLoadsOutOfLoops"
        (applyToOpBlock (\b -> block $ pullConstantLoadsOutOfLoops $ blockStatements b))

pullConstantLoadsOutOfLoops :: [Statement a] -> [Statement a]
pullConstantLoadsOutOfLoops stmts =
  L.concatMap (expandStatement $ pullConstantLoadsOutOfLoop registersWrittenOnce) stmts
  where
    registersWrittenOnce = L.map fst $ L.filter (\(r, writeLocs) -> L.length writeLocs == 1) $ M.toList $ registerWriteLocations $ block stmts

pullConstantLoadsOutOfLoop regsWrittenOnce stmt =
  case isFor stmt of
    True ->
      let constLoads = L.filter (isConstLoad regsWrittenOnce) $ blockStatements $ forBody stmt
          otherStmts = L.filter (\st -> not $ isConstLoad regsWrittenOnce st) $ blockStatements $ forBody stmt in
      constLoads ++ [for (forInductionVariable stmt) (forStart stmt) (forInc stmt) (forEnd stmt) (block otherStmts) (label stmt)]
    False -> [stmt]

isConstLoad regsWrittenOnce stmt =
  isLoad stmt &&
  L.elem (operandWritten stmt) regsWrittenOnce &&
  (isConst $ accessIExpr $ head $ operandsRead stmt)

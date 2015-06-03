module FullLoopUnrolling(fullyUnrollLoop,
                         fullyUnrollAllLoops) where

import Data.List as L

import IndexExpression
import LoopAnalysis
import MiniOperation
import Syntax

fullyUnrollAllLoops =
  optimization
        "FullyUnrollAllLoops"
        (applyToOpBlock (expandBlockStatements tryFullyUnrollLoop))

tryFullyUnrollLoop st =
  case fullyUnrollLoop st of
    Just stmts -> stmts
    Nothing -> [st]

fullyUnrollLoop :: Statement a -> Maybe [Statement a]
fullyUnrollLoop st =
  case isFor st of
    True -> do
      iterSpace <- allIterationsList (forStart st) (forInc st) (forEnd st)
      Just $ L.concatMap (\i -> blockStatements $ subIExprInBlock i (forInductionVariable st) (forBody st)) iterSpace
    False -> Nothing

subIExprInBlock :: IExpr -> String -> Block a -> Block a
subIExprInBlock ie n b = transformBlock (transformStatementIExprs (subIExprForVar ie n)) b

subIExprForVar :: IExpr -> String -> IExpr -> IExpr
subIExprForVar ie varName expr = subIExpr (iVar varName) ie expr

module Optimizations.FullLoopUnrolling(fullyUnrollLoop,
                         fullyUnrollAllLoops) where

import Data.List as L

import Analysis.Loop
import IndexExpression
import Core.MiniOperation
import Core.MiniSyntax

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


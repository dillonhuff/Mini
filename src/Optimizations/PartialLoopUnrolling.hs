module Optimizations.PartialLoopUnrolling(partiallyUnrollAllLoopsBy,
                                          partiallyUnrollBy) where

import Data.List as L

import Analysis.Loop
import Core.IndexExpression
import Core.LoopTransformations
import Core.MiniOperation
import Core.MiniSyntax

partiallyUnrollAllLoopsBy m =
  optimization
        "PartialyUnrollAllLoops"
        (applyToOpBlock (expandBlockStatements (tryPartialyUnrollLoop m)))

tryPartialyUnrollLoop n st =
  case partiallyUnrollLoop n st of
    Just stmts -> stmts
    Nothing -> [st]

partiallyUnrollLoop :: Int -> Statement String -> Maybe [Statement String]
partiallyUnrollLoop n st =
  case partiallyUnrollable n st of
    True -> Just $ partiallyUnrollBy n st
    False -> Nothing

partiallyUnrollable n st =
  isFor st && moreThanNIterations n st && unitIncrement st

module Optimizations.FullLoopUnrolling(tryFullyUnrollLoop,
                                       fullyUnrollLoopsBelow,
                                       fullyUnrollAllLoops) where

import Data.List as L

import Analysis.Loop
import Core.IndexExpression
import Core.LoopTransformations
import Core.MiniOperation
import Core.MiniSyntax

fullyUnrollLoopsBelow maxNumIters =
  optimization
        "FullyUnrollLoopsBelowNumIters"
        (applyToOpBlock (expandBlockStatements $ tryFullyUnrollLoopBelow maxNumIters))

tryFullyUnrollLoopBelow maxNumIters st =
  case isFor st of
    True -> case allIterationsList (forStart st) (forInc st) (forEnd st) of
      Just iters -> case L.length iters <= maxNumIters of
        True -> fullyUnrollLoop iters st
        False -> [st]
      Nothing -> [st]
    False -> [st]

fullyUnrollAllLoops =
  optimization
        "FullyUnrollAllLoops"
        (applyToOpBlock (expandBlockStatements tryFullyUnrollLoop))

tryFullyUnrollLoop st =
  case isFor st of
    True -> case allIterationsList (forStart st) (forInc st) (forEnd st) of
      Just iters -> fullyUnrollLoop iters st
      Nothing -> [st]
    False -> [st]


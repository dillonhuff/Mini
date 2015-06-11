module Optimizations.FullLoopUnrolling(tryFullyUnrollLoop,
                                       fullyUnrollAllLoops) where

import Data.List as L

import Analysis.Loop
import Core.IndexExpression
import Core.LoopTransformations
import Core.MiniOperation
import Core.MiniSyntax

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


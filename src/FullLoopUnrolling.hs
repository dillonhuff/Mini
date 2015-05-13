module FullLoopUnrolling(fullyUnrollLoop) where

import LoopAnalysis
import Syntax

fullyUnrollLoop :: Statement a -> Maybe [Statement a]
fullyUnrollLoop st =
  case isFor st of
    True -> do
      iterSpace <- allIterationsList (forStart st) (forInc st) (forEnd st)
      Nothing --L.concatMap (\i -> transformStatementIExprs (\t -> subIExpr forBody st) iterSpace
    False -> Nothing

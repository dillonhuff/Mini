module PartialLoopUnrolling(partiallyUnrollAllLoopsBy) where

import Data.List as L

import IndexExpression
import LoopAnalysis
import MiniOperation
import Syntax

partiallyUnrollAllLoopsBy m =
  optimization
        "PartialyUnrollAllLoops"
        (applyToOpBlock (expandBlockStatements (tryPartialyUnrollLoop m)))

tryPartialyUnrollLoop n st =
  case partiallyUnrollLoop n st of
    Just stmts -> stmts
    Nothing -> [st]

partiallyUnrollLoop :: Int -> Statement a -> Maybe [Statement a]
partiallyUnrollLoop n st =
  case partiallyUnrollable n st of
    True -> Just $ partiallyUnrollBy n st
--      iterSpace <- allIterationsList (forStart st) (forInc st) (forEnd st)
--      Just $ L.concatMap (\i -> blockStatements $ subIExprInBlock i (forInductionVariable st) (forBody st)) iterSpace
    False -> Nothing

partiallyUnrollable n st =
  isFor st && moreThanNIterations n st && unitIncrement st

partiallyUnrollBy n st =
  [mainLoop]
  where
    mainIVarName = forInductionVariable st
    mainIVar = iVar mainIVarName
    mainLoop = for mainIVarName (forStart st) (iConst n) (iSub (forEnd st) (iConst n)) unrolledBody (label st)
    unrolledBody = block $ L.concatMap (\i -> blockStatements $ subIExprInBlock (iAdd mainIVar (iConst i)) mainIVarName (forBody st)) [0..(n - 1)]
    

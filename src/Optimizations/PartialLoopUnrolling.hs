module Optimizations.PartialLoopUnrolling(partiallyUnrollAllLoopsBy) where

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

partiallyUnrollLoop :: Int -> Statement String -> Maybe [Statement String]
partiallyUnrollLoop n st =
  case partiallyUnrollable n st of
    True -> Just $ partiallyUnrollBy n st
    False -> Nothing

partiallyUnrollable n st =
  isFor st && moreThanNIterations n st && unitIncrement st

partiallyUnrollBy n st =
  [mainLoop, residualLoop]
  where
    mainIVarName = forInductionVariable st
    mainIVar = iVar mainIVarName
    mainEnd = (iAdd (iSub (forEnd st) (iConst n)) (iConst 1))
    mainLoop = for mainIVarName (forStart st) (iConst n) mainEnd unrolledBody (label st)
    unrolledBody = block $ L.concatMap (\i -> blockStatements $ subIExprInBlock (iAdd mainIVar (iConst i)) mainIVarName (forBody st)) [0..(n - 1)]
    residualIVarName = mainIVarName
    residualIVar = iVar residualIVarName
    residualLoop = for residualIVarName residualIVar (iConst 1) (forEnd st) residualBody ((label st) ++ "_u")
    residualBody = block $ blockStatements $ subIExprInBlock residualIVar mainIVarName (forBody st)

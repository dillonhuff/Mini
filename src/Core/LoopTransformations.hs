module Core.LoopTransformations(partiallyUnrollBy,
                                fullyUnrollLoop,
                                unrollLoopsBy2,
                                unrollWithNewLabels) where

import Data.List as L

import Core.IndexExpression
import Core.MiniSyntax

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

fullyUnrollLoop :: [IExpr] -> Statement a -> [Statement a]
fullyUnrollLoop iterSpace st =
  L.concatMap (\i -> blockStatements $ subIExprInBlock i (forInductionVariable st) (forBody st)) iterSpace

unrollWithNewLabels unrollFactor loop =
  L.concatMap (\i -> unrolledBody i loop) [1..unrollFactor]

unrolledBody i loop =
  L.map (\st -> setLabel ((label st) ++ "_iter" ++ show i) st) $ blockStatements $ subIExprInBlock (iConst i) (forInductionVariable loop) (forBody loop)

unrollLoopsBy2 stmts =
  transformStatementList (L.concatMap (expandStatement unrollLoopBy2)) stmts

unrollLoopBy2 stmt =
  case isFor stmt of
    True -> L.concatMap (\i -> blockStatements $ subIExprInBlock i (forInductionVariable stmt) (forBody stmt)) [iConst 0, iConst 1]
    False -> [stmt]

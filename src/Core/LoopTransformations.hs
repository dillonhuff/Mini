module Core.LoopTransformations(partiallyUnrollBy,
                                fullyUnrollLoop,
                                unrollLoopsBy2,
                                unrollWithNewLabels) where

import Data.List as L

import Core.IndexExpression
import Core.MiniSyntax

partiallyUnrollBy n st =
  [mainLoop n st unrolledBody, residualLoop st residualBody]
  where
    mainIVarName = forInductionVariable st
    mainIVar = iVar mainIVarName
    unrolledBody = unrollBlock (\i a -> a) (L.map (\i -> iAdd mainIVar (iConst i)) [0..(n-1)]) (forInductionVariable st) (forBody st)
    residualBody = forBody st

mainLoop n st body =
  let mainIVarName = forInductionVariable st
      mainIVar = iVar mainIVarName
      mainEnd = (iAdd (iSub (forEnd st) (iConst n)) (iConst 1))
      loop = for mainIVarName (forStart st) (iConst n) mainEnd body (label st) in
  loop

residualLoop st body =
  let residualIVarName = forInductionVariable st
      residualIVar = iVar residualIVarName
      resLoop = for residualIVarName residualIVar (iConst 1) (forEnd st) body ((label st) ++ "_u") in
  resLoop

tryFullyUnrollLoop :: [IExpr] -> Statement a -> [Statement a]
tryFullyUnrollLoop iterSpace stmt =
  case isFor stmt of
    True -> fullyUnrollLoop iterSpace stmt
    False -> [stmt]

fullyUnrollLoop :: [IExpr] -> Statement a -> [Statement a]
fullyUnrollLoop iterSpace st =
  blockStatements $ unrollBlock (\i a -> a) iterSpace (forInductionVariable st) $ forBody st

unrollWithNewLabels unrollFactor loop =
  blockStatements $ unrollBlock labF (L.map iConst [1..unrollFactor]) (forInductionVariable loop) $ forBody loop
  where
    labF i a = show a ++ "_iter" ++ show i

unrollLoopsBy2 stmts =
  transformStatementList (L.concatMap (expandStatement (tryFullyUnrollLoop [iConst 0, iConst 1]))) stmts

unrollBlock :: (IExpr -> a -> a) -> [IExpr] -> String -> Block a -> Block a
unrollBlock labFunc iterSpace inductionVar b =
  block $ L.concatMap (\i -> unrollB labFunc i inductionVar b) iterSpace

unrollB :: (IExpr -> a -> a) -> IExpr -> String -> Block a -> [Statement a]
unrollB labFunc iter inductionVar blk =
  let newIExprBlk = subIExprInBlock iter inductionVar blk in
  blockStatements $ transformBlock (\st -> setLabel (labFunc iter (label st)) st) newIExprBlk

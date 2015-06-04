module LoopFusion(fuseAllTopLevelLoopsPossible) where

import IndexExpression
import MiniOperation
import Syntax

fuseAllTopLevelLoopsPossible =
  optimization
        "FuseAllLoopsPossible"
        (applyToOpBlock fuseLoopsInBlock)

fuseLoopsInBlock b =
  let stmts = blockStatements b in
  block $ fuseLoopsInStmtList stmts

fuseLoopsInStmtList [] = []
fuseLoopsInStmtList [stmt] = [stmt]
fuseLoopsInStmtList (s1:s2:rest) =
  case canBeFused s1 s2 of
    True -> fuseLoopsInStmtList $ (fuseLoops s1 s2):rest
    False -> s1 : (fuseLoopsInStmtList (s2:rest))

canBeFused s1 s2 =
  case isFor s1 && isFor s2 of
    True -> sameIterationSpace s1 s2 &&
            allSimpleAccesses s1 &&
            allSimpleAccesses s2 &&
            noWritesOverLap s1 s2
    False -> False

sameIterationSpace s1 s2 =
  forStart s1 == forStart s2 &&
  forEnd s1 == forEnd s2 &&
  sameIncrement s1 s2

allSimpleAccesses forLoop =
  True

noWritesOverLap loop1 loop2 =
  True

sameIncrement s1 s2 = True

fuseLoops loop1 loop2 =
  for (forInductionVariable loop1) (forStart loop1) (forInc loop1) (forEnd loop1)
  (mergeLoopBodies loop1 loop2) (label loop1)

mergeLoopBodies loop1 loop2 =
  let newIVar = iVar $ forInductionVariable loop1
      oldIVar = iVar $ forInductionVariable loop2
      stmts1 = blockStatements $ forBody loop1 in
  block $ stmts1 ++ (blockStatements $ transformBlock (transformStatementIExprs (subIExpr oldIVar newIVar)) $ forBody loop2)
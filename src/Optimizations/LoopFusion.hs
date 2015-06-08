module Optimizations.LoopFusion(fuseAllTopLevelLoopsPossible) where

import Data.List as L

import Analysis.Dependence.Graph
import Analysis.Dependence.RegisterReduction
import Core.IndexExpression
import Core.MiniOperation
import Core.MiniSyntax

fuseAllTopLevelLoopsPossible :: (Ord a, Show a) => Optimization a
fuseAllTopLevelLoopsPossible =
  optimization
        "FuseAllLoopsPossible"
        (applyToOpBlock fuseLoopsInBlock)

fuseLoopsInBlock b =
  let stmts = blockStatements b in
  block $ transformStatementList fuseLoopsInStmtList stmts

fuseLoopsInStmtList [] = []
fuseLoopsInStmtList [stmt] = [stmt]
fuseLoopsInStmtList (s1:s2:rest) =
  case isFor s1 && isFor s2 && sameIterationSpace s1 s2 of
    True -> case tryToFuse s1 s2 of
      Just l -> fuseLoopsInStmtList $ l : rest
      Nothing -> s1 : (fuseLoopsInStmtList (s2:rest))
    False -> s1 : (fuseLoopsInStmtList (s2:rest))

tryToFuse l1 l2 =
  case buildDependenceGraph [fuseLoops l1 l2] of
    Just g -> case noFlowAntiOrOutDeps g l1 l2 of
      True -> Just $ fuseLoops l1 l2
      False -> Nothing
    Nothing -> Nothing

noFlowAntiOrOutDeps g l1 l2 =
  let labs1 = L.map label $ nonLoopStatements l1
      labs2 = L.map label $ nonLoopStatements l2 in
  not $ anyFlowAntiOrOutputDeps g labs1 labs2

sameIterationSpace s1 s2 =
  forStart s1 == forStart s2 &&
  forEnd s1 == forEnd s2 &&
  forInc s1 == forInc s2

noWritesOverlap loop1 loop2 =
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

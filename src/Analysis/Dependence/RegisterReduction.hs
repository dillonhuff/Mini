module Analysis.Dependence.RegisterReduction(buildDependenceGraph,
                                             noLoopCarriedFlowDependenciesInInnerLoop,
                                             unrollWithNewLabels) where

import Data.List as L

import Analysis.Dependence.Graph
import Analysis.Dependence.Register
import Analysis.Loop
import Analysis.RegisterReduction
import Core.IndexExpression
import Core.LoopTransformations
import Core.MiniSyntax

buildDependenceGraph :: (Ord a, Show a) => [Statement a] -> Maybe (DependenceGraph a)
buildDependenceGraph stmts = do
  bufMapRRForm <- reduceToRegisterForm stmts
  registerDependenceGraph $ snd $ bufMapRRForm

noLoopCarriedFlowDependenciesInInnerLoop :: Statement String -> Bool
noLoopCarriedFlowDependenciesInInnerLoop stmt =
  case isFor stmt && noDeeperLoops stmt of
    True -> checkCarriedFlowDeps stmt
    False -> error $ "noLoopCarriedFlowDependenciesInInnerLoop called on non-inner loop " ++ show stmt

checkCarriedFlowDeps innerFor =
  let expandedBody = unrollWithNewLabels 2 innerFor
      firstIterLabels = L.map label $ L.take (div (L.length expandedBody) 2) expandedBody
      secondIterLabels = L.map label $ L.drop (div (L.length expandedBody) 2) expandedBody in
  case buildDependenceGraph expandedBody of
    Just g -> not $ anyFlowDeps g secondIterLabels firstIterLabels
    Nothing -> error $ "Cannot build dependence graph for " ++ show innerFor

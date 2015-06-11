module Analysis.Dependence.RegisterReduction(buildDependenceGraph,
                                             buildLoopDependenceGraph) where

import Data.List as L

import Analysis.Dependence.Graph
import Analysis.Dependence.Register
import Analysis.RegisterReduction
import Core.IndexExpression
import Core.MiniSyntax

buildDependenceGraph :: (Ord a, Show a) => [Statement a] -> Maybe (DependenceGraph a)
buildDependenceGraph stmts = do
  bufMapRRForm <- reduceToRegisterForm stmts
  registerDependenceGraph $ snd $ bufMapRRForm

buildLoopDependenceGraph stmt = do
  bufMapRRForm <- reduceToRegisterForm [stmt]
  registerDependenceGraph $ snd $ bufMapRRForm

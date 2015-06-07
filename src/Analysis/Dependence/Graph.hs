module Analysis.Dependence.Graph(DependenceGraph,
                                 dependenceGraph,
                                 Dependence,
                                 flowDep, antiDep, outputDep, inputDep,
                                 flowDependent, antiDependent) where

import Data.Graph.Inductive as G
import Data.List as L
import Data.Map as M

data DependenceGraph a
     = DependenceGraph (Map a Node) (Gr a Dependence)
       deriving (Eq, Show)

dependenceGraph :: (Show a, Ord a) => [a] -> [(a, a, Dependence)] -> DependenceGraph a
dependenceGraph labels depTriples =
  let labNodePairs = L.zip labels [1..(length labels)]
      nodeLabPairs = L.map (\(x, y) -> (y, x)) labNodePairs
      labNodeMap = M.fromList labNodePairs
      depEdges = L.map (\(l, r, d) -> (findLabel r labNodeMap, findLabel l labNodeMap, d)) depTriples in
  DependenceGraph labNodeMap (insEdges depEdges $ insNodes nodeLabPairs G.empty)

data Dependence
  = Flow
  | Anti
  | Output
  | Input
    deriving (Eq, Ord, Show)

flowDep = Flow
antiDep = Anti
outputDep = Output
inputDep = Input

findLabel :: (Show a, Ord a) => a -> Map a Node -> Node
findLabel l m =
  case M.lookup l m of
    Just n -> n
    Nothing -> error $ "findLabel: " ++ show l ++ " does not exist in " ++ show m

graphNode :: (Ord a, Show a) => a -> DependenceGraph a -> Node
graphNode l (DependenceGraph m _) =
  case M.lookup l m of
    Just n -> n
    Nothing -> error $ "graphNode: " ++ show l ++ " does not exist in " ++ show m

dependenciesOfType :: (Show a, Ord a) => Dependence -> a -> DependenceGraph a -> [Node]
dependenciesOfType d l dg@(DependenceGraph m g) =
  let deps = lsuc g (graphNode l dg) in
  L.map fst $ L.filter (\(_, y) -> y == d) deps      

dependentQuery :: (Show a, Ord a) => Dependence -> DependenceGraph a -> a -> a -> Bool
dependentQuery d depGraph t s =
  L.elem (graphNode t depGraph) (dependenciesOfType d s depGraph)

flowDependent depGraph l1 l2 = dependentQuery Flow depGraph l1 l2

antiDependent depGraph l1 l2 = dependentQuery Anti depGraph l2 l1

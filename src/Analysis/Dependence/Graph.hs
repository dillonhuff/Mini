module Analysis.Dependence.Graph(DependenceGraph,
                                 dependenceGraph,
                                 Dependence,
                                 flowDep, antiDep, outputDep, inputDep,
                                 anyFlowAntiOrOutputDeps, anyFlowDeps,
                                 flowDependent, antiDependent, outputDependent,
                                 carriedFlowDependent,
                                 loopCarriedFlowDeps) where

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
  = Dependence DepType (Maybe LoopStatus)
    deriving (Eq, Ord, Show)

data DepType
  = Flow
  | Anti
  | Output
  | Input
    deriving (Eq, Ord, Show)

data LoopStatus
  = Carried
  | Independent
  | Mixed
    deriving (Eq, Ord, Show)

flowDep = Dependence Flow Nothing
antiDep = Dependence Anti Nothing
outputDep = Dependence Output Nothing
inputDep = Dependence Input Nothing

carriedFlowDep = Dependence Flow (Just Carried)
carriedAntiDep = Dependence Anti (Just Carried)
carriedOuputDep = Dependence Output (Just Carried)
carriedInputDep = Dependence Input (Just Carried)

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

flowDependent depGraph t s = dependentQuery flowDep depGraph t s
antiDependent depGraph t s = dependentQuery antiDep depGraph t s
outputDependent depGraph t s = dependentQuery outputDep depGraph t s

carriedFlowDependent depGraph t s = dependentQuery carriedFlowDep depGraph t s

anyFlowAntiOrOutputDeps g l1 l2 =
  L.or $ L.map (\(t, s) -> antiDependent g t s || flowDependent g t s || outputDependent g t s) [(t, s) | t <- l1, s <- l2]

anyFlowDeps g l1 l2 =
  L.or $ L.map (\(t, s) -> flowDependent g t s) [(t, s) | t <- l1, s <- l2]
  
loopCarriedFlowDeps g t =
  dependenciesOfType carriedFlowDep t g

anyAnti g l1 l2 =
  L.or $ L.map (\(t, s) -> antiDependent g t s) [(t, s) | t <- l1, s <- l2]

anyFlow g l1 l2 =
  L.filter (\(t, s) -> flowDependent g t s) [(t, s) | t <- l1, s <- l2]

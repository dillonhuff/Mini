module Analysis.Dependence(isFlowDependent,
                          isAntiDependent,
                          isOutputDependent,
                          isInputDependent,
                          indexRange,
                          flowDependent, antiDependent,
                          dependenceGraphForVectorInnerLoop) where

import Data.Graph.Inductive as G
import Data.List as L
import Data.Map as M

import MiniOperation
import IndexExpression
import Syntax

isFlowDependent :: (Show a) => [IndexRange] -> Statement a -> Statement a -> Bool
isFlowDependent iRanges t s =
  0 < (L.length $ L.intersectBy (operandsEqual iRanges) [operandWritten s] (operandsRead t))

isAntiDependent :: (Show a) => [IndexRange] -> Statement a -> Statement a -> Bool
isAntiDependent iRanges t s =
  0 < (L.length $ L.intersectBy (operandsEqual iRanges) [operandWritten t] (operandsRead s))

isOutputDependent :: (Show a) => [IndexRange] -> Statement a -> Statement a -> Bool
isOutputDependent iRanges t s =
  operandsEqual iRanges (operandWritten t) (operandWritten s)

isInputDependent :: (Show a) => [IndexRange] -> Statement a -> Statement a -> Bool
isInputDependent iRanges t s =
  0 < (L.length $ L.intersectBy (operandsEqual iRanges) (operandsRead t) (operandsRead s))

operandsEqual :: [IndexRange] -> Operand -> Operand -> Bool
operandsEqual iRanges l r = case l == r of
  True -> True
  False -> case isBufferVal l && isBufferVal r of
    True -> buffersCouldBeEqual iRanges l r
    False -> False

buffersCouldBeEqual iRanges l r =
  bufferName l == bufferName r

data IndexRange
  = IndexRange IExpr IExpr
    deriving (Eq, Ord, Show)

indexRange start end = IndexRange start end

computeDependencies :: (Eq a, Show a) => [Statement a] -> [(Statement a, Statement a, Dependence)]
computeDependencies stmts =
  L.concatMap (\stmt -> statementDeps stmt stmts) stmts

statementDeps :: (Eq a, Show a) => Statement a -> [Statement a] -> [(Statement a, Statement a, Dependence)]
statementDeps st others = L.concatMap (\other -> statementPairDeps st other) $ L.filter (\other -> label other /= label st) others

statementPairDeps l r =
  case isFlowDependent [] l r of
    True -> [(l, r, Flow)]
    False -> []

data DependenceGraph a
     = DependenceGraph (Map a Node) (Gr a Dependence)
       deriving (Eq, Show)

data Dependence
  = Flow
  | Anti
  | Output
  | Input
    deriving (Eq, Ord, Show)

dependenceGraphForVectorInnerLoop :: (Show a, Ord a) => Statement a -> DependenceGraph a
dependenceGraphForVectorInnerLoop forLoopStmt =
  let stmts = nonLoopStatements forLoopStmt
      labNodePairs = L.zip (L.map label stmts) [1..(length stmts)]
      nodeLabPairs = L.map (\(x, y) -> (y, x)) labNodePairs
      depTriples = computeDependencies stmts
      labNodeMap = M.fromList labNodePairs
      depEdges = L.map (\(l, r, d) -> (findNode r labNodeMap, findNode l labNodeMap, d)) depTriples in
  DependenceGraph labNodeMap (insEdges depEdges $ insNodes nodeLabPairs G.empty)

findNode :: (Show a, Ord a) => Statement a -> Map a Node -> Node
findNode st m =
  case M.lookup (label st) m of
    Just n -> n
    Nothing -> error $ "findNode: " ++ show st ++ " does not exist in " ++ show m

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

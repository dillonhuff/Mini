module Analysis.Dependence(isFlowDependent,
                          isAntiDependent,
                          isOutputDependent,
                          isInputDependent,
                          indexRange,
                          flowDependent, antiDependent,
                          dependenceGraph) where

import Data.Graph.Inductive as G
import Data.List as L
import Data.Map as M

import MiniOperation
import IndexExpression
import Syntax

isFlowDependent :: (Show a) => [IndexRange] -> Statement a -> Statement a -> Bool
isFlowDependent iRanges s1 s2 =
  0 < (L.length $ L.intersectBy (operandsEqual iRanges) [operandWritten s1] (operandsRead s2))

isAntiDependent :: (Show a) => [IndexRange] -> Statement a -> Statement a -> Bool
isAntiDependent iRanges s1 s2 =
  0 < (L.length $ L.intersectBy (operandsEqual iRanges) [operandWritten s2] (operandsRead s1))

isOutputDependent :: (Show a) => [IndexRange] -> Statement a -> Statement a -> Bool
isOutputDependent iRanges s1 s2 =
  operandsEqual iRanges (operandWritten s1) (operandWritten s2)

isInputDependent :: (Show a) => [IndexRange] -> Statement a -> Statement a -> Bool
isInputDependent iRanges s1 s2 =
  0 < (L.length $ L.intersectBy (operandsEqual iRanges) (operandsRead s1) (operandsRead s2))

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

computeDependencies :: (Show a) => [Statement a] -> [(Statement a, Statement a, Dependence)]
computeDependencies stmts =
  L.concatMap statementDeps $ L.tails stmts

statementDeps :: (Show a) => [Statement a] -> [(Statement a, Statement a, Dependence)]
statementDeps [] = []
statementDeps [st] = []
statementDeps (st:stmts) = L.concatMap (\otherSt -> statementPairDeps st otherSt) stmts

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

dependenceGraph :: (Show a, Ord a) => Operation a -> DependenceGraph a
dependenceGraph op =
  let stmts = allNonLoopStatementsInOperation op
      labNodePairs = L.zip (L.map label stmts) [1..(length stmts)]
      nodeLabPairs = L.map (\(x, y) -> (y, x)) labNodePairs
      depTriples = computeDependencies stmts
      labNodeMap = M.fromList labNodePairs
      depEdges = L.map (\(l, r, d) -> (findNode l labNodeMap, findNode r labNodeMap, d)) depTriples in
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
dependentQuery d depGraph l1 l2 =
  L.elem (graphNode l2 depGraph) (dependenciesOfType d l1 depGraph)

flowDependent depGraph l1 l2 = dependentQuery Flow depGraph l1 l2
antiDependent depGraph l1 l2 = dependentQuery Anti depGraph l2 l1

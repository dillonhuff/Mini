module DependenceAnalysis(isFlowDependent,
                          isAntiDependent,
                          isOutputDependent,
                          isInputDependent,
                          indexRange,
                          flowDependent,
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

data DependenceGraph a
     = DependenceGraph (Map a Node) (Gr a Dependence)
       deriving (Eq, Show)

data Dependence
  = Flow
  | Anti
  | Output
  | Input
    deriving (Eq, Ord, Show)

dependenceGraph :: (Ord a) => Operation a -> DependenceGraph a
dependenceGraph op =
  let stmts = allNonLoopStatementsInOperation op
      labNodePairs = L.zip (L.map label stmts) [1..(length stmts)]
      nodeLabPairs = L.map (\(x, y) -> (y, x)) labNodePairs in
  DependenceGraph (M.fromList labNodePairs) (insNodes nodeLabPairs G.empty)

graphNode :: (Ord a, Show a) => a -> DependenceGraph a -> Node
graphNode l (DependenceGraph m _) =
  case M.lookup l m of
    Just n -> n
    Nothing -> error $ "graphNode: " ++ show l ++ " does not exist in " ++ show m

flowDependencies :: (Show a, Ord a) => a -> DependenceGraph a -> [Node]
flowDependencies l dg@(DependenceGraph m g) =
  let deps = lsuc g (graphNode l dg) in
  L.map fst $ L.filter (\(_, y) -> y == Flow) deps      

flowDependent :: (Show a, Ord a) => DependenceGraph a -> a -> a -> Bool
flowDependent depGraph l1 l2 =
  L.elem (graphNode l2 depGraph) (flowDependencies l1 depGraph)
  

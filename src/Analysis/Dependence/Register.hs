module Analysis.Dependence.Register(isFlowDependent,
                                    isAntiDependent,
                                    isOutputDependent,
                                    isInputDependent,
                                    indexRange,
                                    flowDependent, antiDependent,
                                    dependenceGraphForVectorInnerLoop) where

import Data.List as L
import Data.Map as M

import Analysis.Dependence.Graph
import Core.IndexExpression
import Core.MiniOperation
import Core.MiniSyntax

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
    True -> [(l, r, flowDep)]
    False -> []

dependenceGraphForVectorInnerLoop :: (Show a, Ord a) => Statement a -> DependenceGraph a
dependenceGraphForVectorInnerLoop forLoopStmt =
  let stmts = nonLoopStatements forLoopStmt
      labels = L.map label stmts
      depTriplesSt = computeDependencies stmts
      depTriples = L.map (\(l, r, d) -> (label l, label r, d)) depTriplesSt in
  dependenceGraph labels depTriples


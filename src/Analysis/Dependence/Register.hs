module Analysis.Dependence.Register(isFlowDependent,
                                    isAntiDependent,
                                    isOutputDependent,
                                    isInputDependent,
                                    registerDependenceGraph) where

import Data.List as L
import Data.Map as M

import Analysis.Dependence.Graph
import Core.IndexExpression
import Core.MiniOperation
import Core.MiniSyntax

isFlowDependent :: (Show a) => [Operand] -> Statement a -> Statement a -> Bool
isFlowDependent opsWrittenBetween t s =
  let possibleDeps = L.intersectBy operandsEqual [operandWritten s] (operandsRead t) in
  L.or $ L.map (\op -> not $ L.elem op opsWrittenBetween) possibleDeps

isAntiDependent :: (Show a) => [Operand] -> Statement a -> Statement a -> Bool
isAntiDependent opsWrittenBetween t s =
  0 < (L.length $ L.intersectBy operandsEqual [operandWritten t] (operandsRead s))

isOutputDependent :: (Show a) => [Operand] -> Statement a -> Statement a -> Bool
isOutputDependent opsWrittenBetween t s =
  operandsEqual (operandWritten t) (operandWritten s)

isInputDependent :: (Show a) => [Operand] -> Statement a -> Statement a -> Bool
isInputDependent opsWrittenBetween t s =
  0 < (L.length $ L.intersectBy operandsEqual (operandsRead t) (operandsRead s))

operandsEqual :: Operand -> Operand -> Bool
operandsEqual l r =
  case isBufferVal l || isBufferVal r of
    True -> error $ "operandsEqual: Comparing buffer value(s) in register dependence analysis " ++ show l ++ " " ++ show r
    False -> l == r

computeDependencies :: (Eq a, Show a) => [Statement a] -> [(Statement a, Statement a, Dependence)]
computeDependencies stmts =
  L.concatMap statementDeps $ L.tails $ L.reverse stmts

statementDeps :: (Eq a, Show a) => [Statement a] -> [(Statement a, Statement a, Dependence)]
statementDeps [] = []
statementDeps [x] = []
statementDeps (st:others) = L.concatMap (\otherList -> stDependsOn st otherList) $ L.tails $ L.reverse others

stDependsOn st [] = []
stDependsOn st [x] = statementPairDeps [] st x
stDependsOn st others = statementPairDeps (L.map operandWritten $ L.tail others) st (head others)

statementPairDeps opsWritten l r =
  L.concatMap (\f -> f opsWritten l r) [fDep, aDep, oDep]

aDep opsWritten l r =
  case isAntiDependent opsWritten l r of
    True -> [(l, r, antiDep)]
    False -> []

fDep opsWritten l r =
  case isFlowDependent opsWritten l r of
    True -> [(l, r, flowDep)]
    False -> []

oDep opsWritten l r =
  case isOutputDependent opsWritten l r of
    True -> [(l, r, outputDep)]
    False -> []

registerDependenceGraph stmts =
  case allRegisterOps stmts of
    True -> Just $ regDepGraph stmts
    False -> Nothing

allRegisterOps stmts =
  L.and $ L.map (\st -> isRegAssign st || isLoadConst st || isBinop st) stmts

regDepGraph stmts =
  let labels = L.map label stmts
      depTriplesSt = computeDependencies stmts
      depTriples = L.map (\(l, r, d) -> (label l, label r, d)) depTriplesSt in
  dependenceGraph labels depTriples

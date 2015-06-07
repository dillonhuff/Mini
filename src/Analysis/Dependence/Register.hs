module Analysis.Dependence.Register(isFlowDependent,
                                    isAntiDependent,
                                    isOutputDependent,
                                    isInputDependent,
                                    flowDependent, antiDependent,
                                    registerDependenceGraph) where

import Data.List as L
import Data.Map as M

import Analysis.Dependence.Graph
import Core.IndexExpression
import Core.MiniOperation
import Core.MiniSyntax

isFlowDependent :: (Show a) => Statement a -> Statement a -> Bool
isFlowDependent t s =
  0 < (L.length $ L.intersectBy operandsEqual [operandWritten s] (operandsRead t))

isAntiDependent :: (Show a) => Statement a -> Statement a -> Bool
isAntiDependent t s =
  0 < (L.length $ L.intersectBy operandsEqual [operandWritten t] (operandsRead s))

isOutputDependent :: (Show a) => Statement a -> Statement a -> Bool
isOutputDependent t s =
  operandsEqual (operandWritten t) (operandWritten s)

isInputDependent :: (Show a) => Statement a -> Statement a -> Bool
isInputDependent t s =
  0 < (L.length $ L.intersectBy operandsEqual (operandsRead t) (operandsRead s))

operandsEqual :: Operand -> Operand -> Bool
operandsEqual l r =
  case isBufferVal l || isBufferVal r of
    True -> error $ "operandsEqual: Comparing buffer value(s) in register dependence analysis " ++ show l ++ " " ++ show r
    False -> l == r

computeDependencies :: (Eq a, Show a) => [Statement a] -> [(Statement a, Statement a, Dependence)]
computeDependencies stmts =
  L.concatMap (\stmt -> statementDeps stmt stmts) stmts

statementDeps :: (Eq a, Show a) => Statement a -> [Statement a] -> [(Statement a, Statement a, Dependence)]
statementDeps st others = L.concatMap (\other -> statementPairDeps st other) $ L.filter (\other -> label other /= label st) others

statementPairDeps l r =
  case isFlowDependent l r of
    True -> [(l, r, flowDep)]
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

module Analysis.Loop(numberOfIterations,
                    allIterationsList,
                    moreThanNIterations,
                    unitIncrement,
                    noDeeperLoops,
                    possibleLoopOrderingsForPerfectNest) where

import Data.List as L

import Core.IndexExpression
import Core.MiniSyntax

numberOfIterations :: IExpr -> IExpr -> IExpr -> Maybe Int
numberOfIterations start inc end = do
  startVal <- ieToConst start
  incVal <- ieToConst inc
  endVal <- ieToConst end
  return $ (div (endVal - startVal) incVal) + 1

allIterationsList :: IExpr -> IExpr -> IExpr -> Maybe [IExpr]
allIterationsList start inc end = do
  startVal <- ieToConst start
  incVal <- ieToConst inc
  endVal <- ieToConst end
  return $ [iConst i | i <- iterSpace startVal incVal endVal]

iterSpace start inc end = L.map (\i -> i * inc + start) [0..(div (end - start) inc)]

moreThanNIterations n st =
  True

unitIncrement st =
  True

noDeeperLoops stmt =
  let nonLoopStmtsInBody = nonLoopStatements stmt
      stmtsInBody = blockStatements $ forBody stmt in
  L.length (L.intersect nonLoopStmtsInBody stmtsInBody) == L.length stmtsInBody

possibleLoopOrderingsForPerfectNest :: Statement a -> [Statement a]
possibleLoopOrderingsForPerfectNest stmt =
  let possibleResults = possibleLoopsForPerfectNest stmt in
  possibleResults

possibleLoopsForPerfectNest :: Statement a -> [Statement a]
possibleLoopsForPerfectNest stmt =
  let (loops, body) = possibleOrderingsForPerfectNest stmt
      possibleOrders = L.permutations loops in
  L.map (\loopOrder -> L.head $ applyLoops loopOrder body) possibleOrders

possibleOrderingsForPerfectNest :: Statement a -> ([[Statement a] -> Statement a], [Statement a])
possibleOrderingsForPerfectNest stmt =
  case isFor stmt of
    True -> getNestLoopsAndBody stmt
    False -> ([], [stmt])

getNestLoopsAndBody forLoop =
  let body = blockStatements $ forBody forLoop in
  case L.length body == 1 && (isFor $ head body) of
    True ->
      let (loops, innerBody) = getNestLoopsAndBody $ head body in
      ((replaceBody forLoop) : loops, innerBody)
    False -> ([replaceBody forLoop], body)

replaceBody forLoop =
  \stmts ->
  for (forInductionVariable forLoop)
      (forStart forLoop)
      (forInc forLoop)
      (forEnd forLoop)
      (block stmts)
      (label forLoop)

applyLoops :: [[Statement a] -> Statement a] -> [Statement a] -> [Statement a]
applyLoops [] body = body
applyLoops (nextLoop:rest) body = applyLoops rest $ [nextLoop body]

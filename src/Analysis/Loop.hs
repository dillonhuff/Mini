module Analysis.Loop(numberOfIterations,
                    allIterationsList,
                    moreThanNIterations,
                    unitIncrement,
                    noDeeperLoops) where

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


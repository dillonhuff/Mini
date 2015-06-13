module Optimizations.SiftLoops(siftLoops,
                               siftLoopsInStmtList) where

import Data.List as L

import Analysis.Basic
import Core.MiniOperation
import Core.MiniSyntax

siftLoops :: (Ord a, Show a) => Optimization a
siftLoops =
  optimization
        "SiftLoops"
        (applyToOpBlock siftLoopsInBlock)

siftLoopsInBlock b =
  let stmts = blockStatements b in
  block $ transformStatementList siftLoopsInStmtList stmts

siftLoopsInStmtList :: (Show a) => [Statement a] -> [Statement a]
siftLoopsInStmtList [] = []
siftLoopsInStmtList [stmt] = [stmt]
siftLoopsInStmtList (st:rest) =
  let siftedRest = siftLoopsInStmtList rest in
  case isFor st of
    True -> tryToSiftLoop st siftedRest
    False -> st : (siftLoopsInStmtList siftedRest)

tryToSiftLoop stmt followingStmts =
  let (nonLoopStmts, rest) = L.break (\st -> isFor st) followingStmts in
  case canSwapOrder nonLoopStmts [stmt] of
    True -> nonLoopStmts ++ [stmt] ++ (siftLoopsInStmtList rest)
    False -> stmt : (siftLoopsInStmtList followingStmts)

canSwapOrder leftStmts rightStmts =
  let leftOps = L.concatMap namesReferenced leftStmts
      rightOps = L.concatMap namesReferenced rightStmts in
  (L.length $ L.intersect leftOps rightOps) == 0

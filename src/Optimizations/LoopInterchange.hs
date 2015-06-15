module Optimizations.LoopInterchange(interchangeLoops) where

import Data.List as L

import Analysis.Loop
import BackEnd.RunBackEnd
import Core.MiniOperation
import Core.MiniSyntax
import Core.SymbolTable

interchangeLoops =
  optimization
        "InterChangeLoops"
        chooseOperationLoopOrder

chooseOperationLoopOrder op =
  let stmts = blockStatements $ getOpBlock op
      n = getOpName op
      st = getMiniOpSymtab op
      opts = getOptimizationsApplied op
      possibleOrders = possibleLoopOrderingsForPerfectNests stmts
      newOps = L.map (\implStmts -> makeOperation n opts st (block implStmts)) possibleOrders
      optimizedOrders = L.map (applyOptimization fuseAndParallelize) newOps in
  leastBuffersAllocated optimizedOrders

leastBuffersAllocated (x:rest) =
  recLeastBuffersAllocated x rest

recLeastBuffersAllocated x [] = x
recLeastBuffersAllocated x (other:rest) =
  case numTempBuffers x > numTempBuffers other of
    True -> recLeastBuffersAllocated other rest
    False -> recLeastBuffersAllocated x rest

numTempBuffers op =
  L.length $ getTmpBuffers $ getMiniOpSymtab op

module Analysis.Basic(registerUsageLocations) where

import Data.List as L
import Data.Map as M

import Core.MiniSyntax

registerUsageLocations b =
  let stmts = L.concatMap nonLoopStatements $ blockStatements b
      regLabelList = L.concatMap (\stmt -> L.map (\op -> (op, label stmt)) $ L.filter (\op -> not $ isBufferVal op) $ allOperands stmt) stmts in
  L.foldl addRegToUseMap M.empty regLabelList

addRegToUseMap m (reg, lab) =
  case M.lookup reg m of
    Just vals -> M.insert reg (lab:vals) m
    Nothing -> M.insert reg [lab] m

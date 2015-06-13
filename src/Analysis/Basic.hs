module Analysis.Basic(registerUsageLocations,
                      registerWriteLocations,
                      allSimpleAccesses,
                      namesReferenced,
                      noLoopsInBlock) where

import Data.List as L
import Data.Map as M

import Core.IndexExpression
import Core.MiniSyntax
import Core.Operand

registerWriteLocations b =
  let stmts = L.concatMap nonLoopStatements $ blockStatements b
      regLabelList = L.concatMap (\stmt -> L.map (\op -> (op, label stmt)) $ L.filter (\op -> not $ isBufferVal op) $ [operandWritten stmt]) stmts in
  L.foldl addRegToUseMap M.empty regLabelList

registerUsageLocations b =
  let stmts = L.concatMap nonLoopStatements $ blockStatements b
      regLabelList = L.concatMap (\stmt -> L.map (\op -> (op, label stmt)) $ L.filter (\op -> not $ isBufferVal op) $ allOperands stmt) stmts in
  L.foldl addRegToUseMap M.empty regLabelList

addRegToUseMap m (reg, lab) =
  case M.lookup reg m of
    Just vals -> M.insert reg (lab:vals) m
    Nothing -> M.insert reg [lab] m

allSimpleAccesses stmt = True
{-  case isFor stmt of
    True -> forAllSimpleAccesses stmt
    False -> basicAllSimpleAccesses stmt-}

forAllSimpleAccesses forLoop =
  let stmts = nonLoopStatements forLoop
      allOps = (L.concatMap operandsRead stmts) ++ (L.map operandWritten stmts) in
  L.and $ L.map (\i -> isConst i || isVar i) $ L.map accessIExpr $ L.filter (\op -> isBufferVal op) allOps

basicAllSimpleAccesses stmt =
  let allOps = (operandWritten stmt) : (operandsRead stmt) in
  L.and $ L.map (\i -> isConst i || isVar i) $ L.map accessIExpr $ L.filter (\op -> isBufferVal op) allOps

namesReferenced stmt =
  case isFor stmt of
    True -> L.concatMap namesReferenced $ blockStatements $ forBody stmt
    False -> L.map operandName $ allOperands stmt

noLoopsInBlock b = L.and $ L.map (\st -> not $ isFor st) $ blockStatements b

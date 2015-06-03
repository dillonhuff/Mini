module TempBufferElimination(eliminateTempBuffers) where

import Data.List as L

import MiniOperation
import SymbolTable
import Syntax

eliminateTempBuffers :: (Show a, Eq a) => Optimization a
eliminateTempBuffers =
  optimization
        "EliminateTempBuffers"
        simpleElimTempBuffers

simpleElimTempBuffers op =
  let tempUsageInfo = tempLoadStoreLocations op in
  applyToOpBlock (updateBlock (simpleRemoveTemps tempUsageInfo)) op

simpleRemoveTemps usageInfo b =
  case noLoopsInBlock b of
    True -> block $ removeTempsFromInnerLoop usageInfo $ blockStatements b
    False -> b

removeTempsFromInnerLoop usageInfo stmts =
  foldr tryToEliminateTemp stmts usageInfo

tryToEliminateTemp (bufName, storeLabels, loadLabels) stmts =
  case L.length storeLabels == 1 of
    True ->
      let storeLabel = head storeLabels
          labs = L.map label stmts
          labsAfterStore = L.drop 1 $ L.dropWhile (\l -> l /= storeLabel) labs
          allLoadsAfterStore = (L.length $ L.intersect loadLabels labsAfterStore) == (L.length loadLabels) in
      case allLoadsAfterStore of
        True -> removeTemp (bufName, storeLabel, loadLabels) stmts
        False -> stmts
    False -> stmts

removeTemp (bufName, storeLabel, loadLabels) stmts =
  let valStored = registerName $ L.head $ operandsRead $ L.head $ L.filter (\st -> label st == storeLabel) stmts
      loadLocations = L.filter (\st -> L.elem (label st) loadLabels) stmts
      receivingRegs = L.map registerName $ L.map operandWritten loadLocations
      remainingStmts = L.filter (\st -> not $ L.elem (label st) (storeLabel:loadLabels)) stmts in
  L.map (multiSubstitution (L.zip (L.replicate (length receivingRegs) valStored) receivingRegs)) remainingStmts

multiSubstitution [] st = st
multiSubstitution ((targetName, resultName):stmts) st =
  multiSubstitution stmts $ substituteName targetName resultName st

tempLoadStoreLocations op =
  let symt = getMiniOpSymtab op
      tmpBufs = getTmpBuffers symt
      stmts = allNonLoopStatementsInOperation op
      loads = L.filter isLoad stmts
      stores = L.filter isStore stmts in
  L.map (createUseTriple loads stores) tmpBufs

createUseTriple loads stores bufName =
  let loadLabels = L.map label $ L.filter (loadsFromBuffer bufName) loads
      storeLocs = L.filter (storesToBuffer bufName) stores
      storeLabels = L.map label storeLocs in
  (bufName, storeLabels, loadLabels)
      

loadsFromBuffer bufName ld = L.elem bufName $ L.map bufferName $ L.filter isBufferVal $ operandsRead ld

storesToBuffer bufName st = bufName == (bufferName $ operandWritten st)

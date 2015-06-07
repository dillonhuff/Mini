module Optimizations.TempBufferElimination(eliminateTempBuffers) where

import Control.Monad.State.Lazy
import Data.List as L

import MiniOperation
import MiniSyntax
import SymbolTable

eliminateTempBuffers :: (Show a, Eq a) => Optimization a
eliminateTempBuffers =
  optimization
        "EliminateTempBuffers"
        elimTempBuffersM

elimTempBuffersM op =
  let st = getMiniOpSymtab op
      b = getOpBlock op
      opts = getOptimizationsApplied op
      (newOpBlock, newSt) = (runState $ elimTmps b) st in
  makeOperation (getOpName op) opts newSt newOpBlock

elimTmps :: (Eq a) => Block a -> State MiniSymtab (Block a)
elimTmps b = do
  symtab <- get
  updateBlockM (simpleElimTempBuffersM (tempLoadStoreLocations symtab b)) b

simpleElimTempBuffersM usageInfo b =
  case noLoopsInBlock b of
    True -> do
      newStmts <- removeTempsFromInnerLoop usageInfo $ blockStatements b
      return $ block newStmts
    False -> return b

removeTempsFromInnerLoop :: (Eq a) => [(String, [a], [a])] -> [Statement a] -> State MiniSymtab [Statement a]
removeTempsFromInnerLoop usageInfo stmts =
  foldM (\stmts info -> tryToEliminateTemp info stmts) stmts usageInfo

tryToEliminateTemp :: (Eq a) => (String, [a], [a]) -> [Statement a] -> State MiniSymtab [Statement a]
tryToEliminateTemp (bufName, storeLabels, loadLabels) stmts =
  case L.length storeLabels == 1 of
    True ->
      let storeLabel = head storeLabels
          labs = L.map label stmts
          labsAfterStore = L.drop 1 $ L.dropWhile (\l -> l /= storeLabel) labs
          allLoadsAfterStore = (L.length $ L.intersect loadLabels labsAfterStore) == (L.length loadLabels) in
      case allLoadsAfterStore of
        True -> let newStmts = removeTemp (bufName, storeLabel, loadLabels) stmts in
          do
            st <- get
            put $ removeSymbol bufName st
            return newStmts
        False -> return stmts
    False -> return stmts

removeTemp (bufName, storeLabel, loadLabels) stmts =
  let valStored = registerName $ L.head $ operandsRead $ L.head $ L.filter (\st -> label st == storeLabel) stmts
      loadLocations = L.filter (\st -> L.elem (label st) loadLabels) stmts
      receivingRegs = L.map registerName $ L.map operandWritten loadLocations
      remainingStmts = L.filter (\st -> not $ L.elem (label st) (storeLabel:loadLabels)) stmts in
  L.map (multiSubstitution (L.zip (L.replicate (length receivingRegs) valStored) receivingRegs)) remainingStmts

multiSubstitution [] st = st
multiSubstitution ((targetName, resultName):rest) st =
  multiSubstitution rest $ substituteName targetName resultName st

tempLoadStoreLocations symt blk =
  let tmpBufs = getTmpBuffers symt
      stmts = L.concatMap nonLoopStatements $ blockStatements blk
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

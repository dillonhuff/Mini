module Optimizations.ArrayCompaction(compactArrays) where

import Data.List as L
import Data.Map as M

import Analysis.RegisterReduction
import Analysis.Liveness.Register
import Core.IndexExpression
import Core.MiniOperation
import Core.MiniSyntax
import Core.Operand
import Core.SymbolTable

compactArrays =
  optimization
        "CompactArrays"
        compactSizeOneArrays

compactSizeOneArrays op =
  let st = getMiniOpSymtab op
      b = getOpBlock op
      bufsToCompactWithInfo = compactableBuffers st $ blockStatements b
      newSt = L.foldr replaceWithScalar st bufsToCompactWithInfo
      newOpBlock = L.foldr replaceBufWithScalar b $ L.map fst bufsToCompactWithInfo in
  makeOperation (getOpName op) (getOptimizationsApplied op) newSt newOpBlock

compactableBuffers st stmts =
  case reduceToRegisterForm stmts of
    Just (bufRegMap, rrForm) -> oneEntryTempBuffers st bufRegMap rrForm
    Nothing -> []

oneEntryTempBuffers st bufRegPairs rrForm = 
  let tmps = getTmpBuffers st
      lRanges = liveRanges rrForm
      tmpBufRegPairs = L.filter (\(bufAcc, reg) -> L.elem (bufferName bufAcc) tmps) bufRegPairs
      tmpsToCompact = L.filter (\tmp -> oneRegNeeded tmp lRanges tmpBufRegPairs) tmps
      info = L.map (\n -> getMiniSymInfo n id st) tmpsToCompact in
  L.zip tmpsToCompact info

oneRegNeeded tmp lRanges tmpBufRegPairs =
  let regsUsed = L.nub $ L.map snd $ L.filter (\(bufAcc, reg) -> bufferName bufAcc == tmp) tmpBufRegPairs
      regRanges = L.map (\n -> liveRange n lRanges) regsUsed in
  (L.length $ L.filter (\(x, y) -> rangesOverlap x y) $ L.filter (\(x, y) -> x /= y) [(l, r) | l <- regRanges, r <- regRanges]) == 0
    
getTempBuffersOfSizeOneWithInfo st =
  let tmpBufsOfSizeOne = L.filter (\b -> bufSize b st == iConst 1) $ getTmpBuffers st
      info = L.map (\n -> getMiniSymInfo n id st) tmpBufsOfSizeOne in
  L.zip tmpBufsOfSizeOne info

replaceWithScalar (bufName, bufInfo) st =
  addEntry bufName (newSymbolInfo bufName st) st

newSymbolInfo bufName st =
  let bufDataType = bufType $ getMiniSymInfo bufName symType st in
  symInfo bufDataType local
      
replaceBufWithScalar bufName block =
  transformBlock (compactBuffer bufName) block

compactBuffer bufName stmt =
  case isLoad stmt of
    True -> compactBufferLoad bufName stmt
    False -> case isStore stmt of
      True -> compactBufferStore bufName stmt
      False -> stmt

compactBufferStore bufName st =
  case bufferName (operandWritten st) == bufName of
    True -> regAssign bufName (registerName $ head $ operandsRead st) (label st)
    False -> st

compactBufferLoad bufName ld =
  case bufferName (head $ operandsRead ld) == bufName of
    True -> regAssign (registerName $ operandWritten ld) bufName (label ld)
    False -> ld

bufSize b st =
  let bufSize = Core.SymbolTable.getBufferSize b st in
  evaluateIExprConstants bufSize

module Core.LoopTransformations(partiallyUnrollBy,
                                partiallyUnrollAndIntersperse,
                                fullyUnrollLoop,
                                unrollLoopsBy2,
                                unrollWithNewLabels) where

import Data.List as L

import Core.IndexExpression
import Core.MiniSyntax
import Core.Operand
import Core.SymbolTable

partiallyUnrollAndIntersperse :: MiniSymtab -> Int -> Statement String -> (MiniSymtab, [Statement String])
partiallyUnrollAndIntersperse symtab n st =
  let mainIVar = iVar $ forInductionVariable st
      stmtsList = replicateBlockStmts (\i a -> show a ++ "_iter" ++ show i) (L.map (\i -> iAdd mainIVar (iConst i)) [0..(n-1)]) (forInductionVariable st) $ forBody st
      stmtsListExecNum = L.zip stmtsList [1..n]
      subStmtsAndRegs = L.map (\(stmts, i) -> addRegisterSuffix ("_is" ++ show i) symtab stmts) stmtsListExecNum
      subStmts = L.map snd subStmtsAndRegs
      allRegs = L.concatMap fst subStmtsAndRegs
      newSymtab = multiAddVar allRegs symtab
      bodyStmts = intersperseStmts subStmts
      mainBody = block bodyStmts in
  (newSymtab, [mainLoop n st mainBody, residualLoop st $ forBody st])

intersperseStmts :: [[Statement a]] -> [Statement a]
intersperseStmts stmtsList =
  case L.and $ L.map (\stmts -> L.length stmts == 0) stmtsList of
    True -> []
    False -> (L.concatMap (\stmts -> [L.head stmts]) stmtsList) ++ (intersperseStmts $ L.map L.tail stmtsList)

addRegisterSuffix :: String -> MiniSymtab -> [Statement a] -> ([(String, String)], [Statement a])
addRegisterSuffix suffix symtab stmts =
  let allStmts = L.concatMap nonLoopStatements stmts
      regsWritten = L.map registerName $ L.filter (\op -> not $ isBufferVal op) $ L.map operandWritten allStmts
      regsWithNewNames = L.zip regsWritten $ L.map (\r -> r ++ suffix) regsWritten
      newStmts = L.map (transformStatement (multiSubstitution regsWithNewNames)) stmts in
  (regsWithNewNames, newStmts)

multiAddVar [] st = st
multiAddVar ((old, new):rest) st =
  multiAddVar rest $ addEntry new (getMiniSymInfo old id st) st
  
partiallyUnrollBy n st =
  [mainLoop n st unrolledBody, residualLoop st $ forBody st]
  where
    mainIVarName = forInductionVariable st
    mainIVar = iVar mainIVarName
    unrolledBody = unrollBlock (\i a -> a) (L.map (\i -> iAdd mainIVar (iConst i)) [0..(n-1)]) (forInductionVariable st) (forBody st)
    residualBody = forBody st

mainLoop n st body =
  let mainIVarName = forInductionVariable st
      mainIVar = iVar mainIVarName
      mainEnd = (iAdd (iSub (forEnd st) (iConst n)) (iConst 1))
      loop = for mainIVarName (forStart st) (iConst n) mainEnd body (label st) in
  loop

residualLoop st body =
  let residualIVarName = forInductionVariable st
      residualIVar = iVar residualIVarName
      resLoop = for residualIVarName residualIVar (iConst 1) (forEnd st) body ((label st) ++ "_u") in
  resLoop

tryFullyUnrollLoop :: [IExpr] -> Statement a -> [Statement a]
tryFullyUnrollLoop iterSpace stmt =
  case isFor stmt of
    True -> fullyUnrollLoop iterSpace stmt
    False -> [stmt]

fullyUnrollLoop :: [IExpr] -> Statement a -> [Statement a]
fullyUnrollLoop iterSpace st =
  blockStatements $ unrollBlock (\i a -> a) iterSpace (forInductionVariable st) $ forBody st

unrollWithNewLabels unrollFactor loop =
  blockStatements $ unrollBlock labF (L.map iConst [1..unrollFactor]) (forInductionVariable loop) $ forBody loop
  where
    labF i a = show a ++ "_iter" ++ show i

unrollLoopsBy2 stmts =
  transformStatementList (L.concatMap (expandStatement (tryFullyUnrollLoop [iConst 0, iConst 1]))) stmts

unrollBlock :: (IExpr -> a -> a) -> [IExpr] -> String -> Block a -> Block a
unrollBlock labFunc iterSpace inductionVar b =
  block $ L.concatMap (\i -> unrollB labFunc i inductionVar b) iterSpace

replicateBlockStmts labFunc iterSpace inductionVar blk =
  L.map (\i -> unrollB labFunc i inductionVar blk) iterSpace
  
unrollB :: (IExpr -> a -> a) -> IExpr -> String -> Block a -> [Statement a]
unrollB labFunc iter inductionVar blk =
  let newIExprBlk = subIExprInBlock iter inductionVar blk in
  blockStatements $ transformBlock (\st -> setLabel (labFunc iter (label st)) st) newIExprBlk

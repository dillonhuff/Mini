module Testing.TestHarness(cTestHarness,
                   parseTimingResults) where

import Data.List as L
import Data.Map as M

import BackEnd.CGen
import Testing.EvaluationResult
import IndexExpression
import MiniOperation
import SymbolTable hiding (getBufferSize)
import MiniSyntax
import SystemSettings

cTestHarness :: (Show a) => a -> Map String Int -> String -> Maybe (Operation a) -> [Operation a] -> String
cTestHarness dummyAnn indexVals opName (Just scImp) implsToTime = 
  let opNames = L.map getOpName implsToTime in
  L.concatMap (\decl -> (prettyPrint 0 decl) ++ "\n") $
  prelude dummyAnn (scImp:implsToTime) ++
  [sanityCheckFunc dummyAnn indexVals scImp implsToTime,
   timingFunc dummyAnn indexVals implsToTime,
   mainFunc dummyAnn (dataFileName opName)]
cTestHarness _ _ _ _ _ = error "Are you sure you don't want to do a sanity check?"

parseTimingResults :: String -> Map String EvaluationResult
parseTimingResults str =
  let (scLines, rtLines) = splitResultsIntoSanityCheckAndTimeSections str
      scResults = parseSCResults scLines
      rtResults = parseRTResults rtLines in
  M.fromList $ mergeResults scResults rtResults

splitResultsIntoSanityCheckAndTimeSections str =
  let strLines = L.lines str
      scLines = L.takeWhile (\l -> l /= scTimingSeparator) strLines
      rtLines = L.drop 1 (L.dropWhile (\l -> l /= scTimingSeparator) strLines) in
  (scLines, rtLines)

scTimingSeparator = "#TIMING_RESULTS"

mergeResults :: [(String, Bool)] -> [(String, Double)] -> [(String, EvaluationResult)]
mergeResults [] _ = []
mergeResults ((n, b):scRest) rtRes =
  case L.lookup n rtRes of
    Just t -> (n, evaluationResult t b) : (mergeResults scRest rtRes)
    Nothing -> error $ "Could not find " ++ n ++ " in " ++ show rtRes

parseRTResults :: [String] -> [(String, Double)]
parseRTResults [] = []
parseRTResults (n:avgCyclesPerRun:rest) = (n, read avgCyclesPerRun) : (parseRTResults rest)

parseSCResults :: [String] -> [(String, Bool)]
parseSCResults [] = []
parseSCResults (n:passFail:rest) = (n, if passFail == "passed" then True else False):(parseSCResults rest)
parseSCResults other = error $ "parseSCResults failed with " ++ show other

prelude :: a -> [Operation a] -> [CTopLevelItem a]
prelude dummyAnn impls =
  let cImplFuncs = L.map (toCFunc dummyAnn) impls
      includes = [cInclude "<stdio.h>", cInclude "<stdlib.h>", cInclude "<string.h>", cInclude "\"mini_utilities.h\""] in
  includes ++ cImplFuncs

mainFunc :: a -> String -> CTopLevelItem a
mainFunc dummyAnn dataFileName =
  cFuncDecl cInt "main" [] $
            cBlock [(cPtr cFILE, "data_file")]
                   [cExprSt (cAssign (cVar "data_file") (cFuncall "fopen" [cVar ("\"" ++ dataFileName ++ "\""), cVar "\"w\""])) dummyAnn,
                    cExprSt (cFuncall "sanity_check_impls" [cVar "data_file"]) dummyAnn,
                    cExprSt (cFuncall "time_impls" [cVar "data_file"]) dummyAnn,
                    cExprSt (cFuncall "fclose" [cVar "data_file"]) dummyAnn,
                    cReturn (cIntLit 0) dummyAnn]

sanityCheckFunc :: a -> Map String Int -> Operation a -> [Operation a] -> CTopLevelItem a
sanityCheckFunc dummyAnn indexVals scImp implsToCheck =
  cFuncDecl cVoid "sanity_check_impls" [(cPtr cFILE, "df")] $ cBlock argDecls (scStatements dummyAnn indexVals scImp implsToCheck)
  where
    argumentBufferDecls = scBufferDecls scImp
    indexDecls = L.map (\(name, tp) -> (toCType tp, name)) $ getIndexArgs scImp
    argDecls = indexDecls ++ argumentBufferDecls

scBufferDecls :: Operation a -> [(CType, String)]
scBufferDecls scImp = argumentBufferDecls
  where
    args = getBufferArgs scImp
    savedBufferDecls = L.map (\(name, tp) -> (toCType tp, name)) args
    refBufferDecls = L.map (\(cType, n) -> (cType, n ++ "_ref")) savedBufferDecls
    testBufferDecls = L.map (\(cType, n) -> (cType, n ++ "_test")) savedBufferDecls
    argumentBufferDecls = savedBufferDecls ++ refBufferDecls ++ testBufferDecls

scStatements :: a -> Map String Int -> Operation a -> [Operation a] -> [CStmt a]
scStatements dummyAnn indexVals scImp implsToCheck = indexAssigns ++ bufferAllocs ++ refImplSetup ++ refImplTestImplComparisons ++ bufferDeallocs
  where
    indexAssigns = L.map (\(name, _) -> indexAssignSt dummyAnn name indexVals) $ getIndexArgs scImp
    bufferAllocs = allocSCBufferStmts dummyAnn scImp
    buffersToDealloc = L.map snd $ scBufferDecls scImp
    bufferDeallocs = L.map (\n -> cExprSt (cFuncall "free" [cVar n]) dummyAnn) buffersToDealloc
    refImplSetup = referenceImplSetup dummyAnn scImp
    refImplTestImplComparisons = referenceImplTestImplComparisons dummyAnn scImp implsToCheck

indexAssignSt :: a -> String -> Map String Int -> CStmt a
indexAssignSt dummyAnn varName indexVals =
  case M.lookup varName indexVals of
    Just val -> cExprSt (cAssign (cVar varName) (cIntLit val)) dummyAnn
    Nothing -> error $ varName ++ " does not exist in index value list " ++ show indexVals

allocSCBufferStmts :: a -> Operation a -> [CStmt a]
allocSCBufferStmts dummyAnn scImp = allocStmts
  where
    argBufs = getBufferArgs scImp
    argBufNames = L.map fst argBufs
    argBufSizes = L.map (\x -> getBufferSize x scImp) argBufNames
    argBufCSizes = L.map (\s -> iExprToCExpr s) argBufSizes
    argBufCDecls = L.map (\(n, tp) -> (getReferencedType $ toCType tp, n)) argBufs
    argBufCDeclsWSize = L.zip argBufCDecls argBufCSizes
    refBufCDeclsWSize = L.map (\((tp, n), sz) -> ((tp, n ++ "_ref"), sz)) argBufCDeclsWSize
    testBufCDeclsWSize = L.map (\((tp, n), sz) -> ((tp, n ++ "_test"), sz)) argBufCDeclsWSize
    allArgBufs = argBufCDeclsWSize ++ refBufCDeclsWSize ++ testBufCDeclsWSize
    allocStmts = L.map (\((tp, n), sz) -> cExprSt (cAssign (cVar n) (cFuncall "malloc" [cMul (cSizeOf tp) sz])) dummyAnn) allArgBufs

timingFunc :: a -> Map String Int -> [Operation a] -> CTopLevelItem a
timingFunc _ _ [] = error $ "no implementations to time in timingFunc"
timingFunc dummyAnn indexVals implsToTime =
  cFuncDecl cVoid "time_impls" [(cPtr cFILE, "df")] $ cBlock [] testBlocks
  where
    testBlocks = L.map (testBlockStmts dummyAnn indexVals) implsToTime

testBlockStmts :: a -> Map String Int -> Operation a -> CStmt a
testBlockStmts dummyAnn indexVals imp =
  cBlockSt varDecls blkCode dummyAnn
  where
    indexDecls = L.map (\(name, tp) -> (toCType tp, name)) $ getIndexArgs imp
    bufDecls = L.map (\(name, tp) -> (toCType tp, name)) $ getBufferArgs imp
    timeVarDecls = [(cULongLong, "start"), (cULongLong, "end"), (cULongLong, "total_cycles"), (cULongLong, "lvar"),
                    (cULongLong, "num_runs"), (cDouble, "avg_cycles_per_run")]
    varDecls = bufDecls ++ timeVarDecls ++ indexDecls
    blkCode = testBlockCode dummyAnn indexVals imp

testBlockCode :: a -> Map String Int -> Operation a -> [CStmt a]
testBlockCode dummyAnn indexVals imp =
  (L.map (\(name, _) -> indexAssignSt dummyAnn name indexVals) $ getIndexArgs imp) ++
  setupCode dummyAnn imp ++
  timingLoops dummyAnn imp ++
  fileIOCode dummyAnn imp ++
  bufferFreeingCode dummyAnn imp

setupCode dummyAnn imp =
  [cExprSt (cFuncall "fprintf" [cVar "df", cVar ("\"" ++ scTimingSeparator ++ "\\n\"")]) dummyAnn] ++
  (bufferAllocationCode dummyAnn imp) ++
  setArgsToRandValues dummyAnn imp
timingLoops dummyAnn imp = countRunsWhile dummyAnn imp ++ timeForRuns dummyAnn imp
fileIOCode dummyAnn imp =
  [cExprSt (cAssign (cVar "avg_cycles_per_run") (cDiv (cSub (cVar "end") (cVar "start")) (cCast cDouble (cVar "num_runs")))) dummyAnn,
   cExprSt (cFuncall "fprintf" [cVar "df", cVar ("\"" ++ getOpName imp ++ "\\n\"")]) dummyAnn,
   cExprSt (cFuncall "fprintf" [cVar "df", cVar "\"%f\\n\"", cVar "avg_cycles_per_run"]) dummyAnn]
  
bufferFreeingCode dummyAnn imp= bufferDeallocs
  where
    buffersToDealloc = L.map (\(name, _) -> name) $ getBufferArgs imp
    bufferDeallocs = L.map (\n -> cExprSt (cFuncall "free" [cVar n]) dummyAnn) buffersToDealloc

countRunsWhile dummyAnn imp =
  [cExprSt (cAssign (cVar "num_runs") (cIntLit 0)) dummyAnn,
   cExprSt (cAssign (cVar "total_cycles") (cIntLit 0)) dummyAnn,
   cWhile (cLEQ (cVar "total_cycles") (cIntLit minCycleCount)) (whileTimeBlock dummyAnn imp) dummyAnn]

minCycleCount = 100000000

whileTimeBlock dummyAnn imp = cBlock []
  [cExprSt (cAssign (cVar "start") (cFuncall "rdtsc" [])) dummyAnn,
   cExprSt (cFuncall (getOpName imp) (L.map (\(n, _) -> cVar n) $ getOpArguments imp)) dummyAnn,
   cExprSt (cAssign (cVar "end") (cFuncall "rdtsc" [])) dummyAnn,
   cExprSt (cAssign (cVar "total_cycles") (cAdd (cVar "total_cycles") (cSub (cVar "end") (cVar "start")))) dummyAnn,
   cExprSt (cAssign (cVar "num_runs") (cAdd (cVar "num_runs") (cIntLit 1))) dummyAnn]

timeForRuns dummyAnn imp =
  [cExprSt (cAssign (cVar "start") (cFuncall "rdtsc" [])) dummyAnn,
   cFor (cAssign (cVar "lvar") (cIntLit 1)) (cLEQ (cVar "lvar") (cVar "num_runs")) (cAssign (cVar "lvar") (cAdd (cVar "lvar") (cIntLit 1)))
        (cBlock [] [cExprSt (cFuncall (getOpName imp) (L.map (\(n, _) -> cVar n) $ getOpArguments imp)) dummyAnn]) dummyAnn,
   cExprSt (cAssign (cVar "end") (cFuncall "rdtsc" [])) dummyAnn] ++
  (L.map (\(n, _) -> cExprSt (cFuncall "print_first_byte" [cVar n]) dummyAnn) $ getBufferArgs imp)

bufferAllocationCode dummyAnn imp = allocStmts
    where
      argBufs = getBufferArgs imp
      argBufNames = L.map fst argBufs
      argBufSizes = L.map (\x -> getBufferSize x imp) argBufNames
      argBufCSizes = L.map (\s -> iExprToCExpr s) argBufSizes
      argBufCDecls = L.map (\(n, tp) -> (getReferencedType $ toCType tp, n)) argBufs
      argBufCDeclsWSize = L.zip argBufCDecls argBufCSizes
      allocStmts = L.map (\((tp, n), sz) -> cExprSt (cAssign (cVar n) (cFuncall "malloc" [cMul (cSizeOf tp) sz])) dummyAnn) argBufCDeclsWSize


referenceImplSetup :: a -> Operation a -> [CStmt a]
referenceImplSetup dummyAnn scImp = setArgsToRand ++ copyArgsToRefs ++ [callSCImp]
  where
    setArgsToRand = setArgsToRandValues dummyAnn scImp
    copyArgsToRefs = copyArgsTo dummyAnn "_ref" scImp
    callSCImp = callWithBufferSuffix dummyAnn "_ref" scImp

callWithBufferSuffix dummyAnn suffix imp =
  let args = L.map (\(n, tp) -> cVar (if isBuffer tp then n ++ suffix else n)) $ getOpArguments imp in
  cExprSt (cFuncall (getOpName imp) args) dummyAnn

copyArgsTo :: a -> String -> Operation a -> [CStmt a]
copyArgsTo dummyAnn suffix scImp = copyArgStmts
  where
    args = getBufferArgs scImp
    copyArgStmts = L.map (\(n, tp) -> cExprSt (cFuncall "memcpy" [cVar (n ++ suffix), cVar n, cMul (cSizeOf $ getReferencedType $ toCType tp) (iExprToCExpr $ getBufferSize n scImp)]) dummyAnn) args

setArgsToRandValues :: a -> Operation a -> [CStmt a]
setArgsToRandValues dummyAnn scImp = randValStmts
  where
    args = getBufferArgs scImp
    argsWSizes = L.map (\(n, tp) -> ((n, toCType tp), iExprToCExpr $ getBufferSize n scImp)) args
    randValStmts = L.map (setArgToRandValuesCode dummyAnn) argsWSizes

setArgToRandValuesCode :: a -> ((String, CType), CExpr) -> CStmt a
setArgToRandValuesCode dummyAnn ((name, tp), sz) =
  case getReferencedType tp == cDouble of
    True -> cExprSt (cFuncall "rand_doubles" [cVar name, sz]) dummyAnn
    False -> case getReferencedType tp == cFloat of
      True -> cExprSt (cFuncall "rand_floats" [cVar name, sz]) dummyAnn
      False -> error $ "Unrecognized type in setArgToRandValuesCode " ++ show tp

referenceImplTestImplComparisons :: a -> Operation a -> [Operation a] -> [CStmt a]
referenceImplTestImplComparisons dummyAnn scImp implsToCheck = compareStmts
  where
    compareStmts = L.map (\imp -> compareImpls dummyAnn scImp imp) implsToCheck

compareImpls :: a -> Operation a -> Operation a -> CStmt a
compareImpls dummyAnn scImp imp = cmpBlk
  where
    args = getBufferArgs scImp
    decls = L.map (\(n, tp) -> (cInt, n ++ "_sc_res")) args
    copyStmts = copyArgsTo dummyAnn "_test" scImp
    writeOpName = cExprSt (cFuncall "fprintf" [cVar "df", cVar ("\"" ++ getOpName imp ++ "\\n\"")]) dummyAnn
    blkStmts = copyStmts ++ [callWithBufferSuffix dummyAnn "_test" imp, writeOpName] ++ scResultCode dummyAnn args scImp
    cmpBlk = cBlockSt decls blkStmts dummyAnn

scResultCode :: a -> [(String, Type)] -> Operation a -> [CStmt a]
scResultCode dummyAnn args imp = asgResults ++ [writeOutput]
  where
    asgResults = L.map (\(n, tp) -> setSCResVar dummyAnn n (getReferencedType $ toCType tp) (iExprToCExpr $ getBufferSize n imp)) args
    resVars = L.map (\(n, tp) -> cVar (n ++ "_sc_res")) args
    resExpr = orExprs resVars
    writeOutput = cIfThenElse resExpr
                              (cBlock [] [cExprSt (cFuncall "fprintf" [cVar "df", cVar "\"FAILED\\n\""]) dummyAnn])
                              (cBlock [] [cExprSt (cFuncall "fprintf" [cVar "df", cVar "\"passed\\n\""]) dummyAnn])
                              dummyAnn

orExprs [e] = e
orExprs (e2:rest) = cOr e2 $ orExprs rest

setSCResVar :: a -> String -> CType -> CExpr -> CStmt a
setSCResVar dummyAnn n tp sizeExpr =
  case tp == cDouble of
    True -> cExprSt (cAssign (cVar (n ++ "_sc_res")) (cFuncall "test_buffer_diff_double" [cVar (n ++ "_ref"), cVar (n ++ "_test"), sizeExpr])) dummyAnn
    False -> case tp == cFloat of
      True -> cExprSt (cAssign (cVar (n ++ "_sc_res")) (cFuncall "test_buffer_diff_float" [cVar (n ++ "_ref"), cVar (n ++ "_test"), sizeExpr])) dummyAnn
      False -> error $ "Unrecognized type " ++ show tp ++ " in setSCResVar"
  

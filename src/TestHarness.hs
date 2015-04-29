module TestHarness(cTestHarness,
                   parseTimingResults,
                   EvaluationResult) where

import Data.List as L
import Data.Map as M

import CGen
import Syntax
import SystemSettings

data EvaluationResult
  = EvaluationResult [Int] Bool
    deriving (Eq, Ord, Show)

evaluationResult = EvaluationResult

cTestHarness :: (Show a) => a -> String -> Maybe (Operation a) -> [Operation a] -> String
cTestHarness dummyAnn fileName (Just scImp) implsToTime = 
  let opNames = L.map getOpName implsToTime
      args = getOpArguments scImp in
  L.concatMap (\decl -> (prettyPrint 0 decl) ++ "\n") $
  prelude (scImp:implsToTime) ++
  [sanityCheckFunc dummyAnn scImp implsToTime,
   timingFunc implsToTime,
   mainFunc dummyAnn]
cTestHarness _ _ _ _ = error "Are you sure you don't want to do a sanity check?"

parseTimingResults :: String -> Map String EvaluationResult
parseTimingResults str =
  let strLines = L.lines str
      scLines = L.takeWhile (\l -> l /= scTimingSeparator) strLines in
  M.fromList $ parseSCResults scLines

scTimingSeparator = "#"

parseSCResults :: [String] -> [(String, EvaluationResult)]
parseSCResults [] = []
parseSCResults (n:passFail:rest) = (n, evaluationResult [] $ if passFail == "passed" then True else False):(parseSCResults rest)
parseSCResults other = error $ "parseSCResults failed with " ++ show other

createDataFileString fileName = "\tFILE* fp = fopen(\"" ++ dataFileName fileName ++ "\", \"w\");\n"
closeDataFileString fileName = "\tfclose(fp);\n"

prelude :: [Operation a] -> [CTopLevelItem a]
prelude impls =
  let cImplFuncs = L.map toCFunc impls
      includes = [cInclude "<stdio.h>", cInclude "<stdlib.h>", cInclude "<string.h>", cInclude "\"mini_utilities.h\""] in
  includes ++ cImplFuncs

mainFunc :: a -> CTopLevelItem a
mainFunc dummyAnn =
  cFuncDecl cInt "main" [] $
            cBlock [(cPtr cFILE, "data_file")]
                   [cExprSt (cFuncall "sanity_check_impls" [cVar "data_file"]) dummyAnn,
                    cExprSt (cFuncall "time_impls" [cVar "data_file"]) dummyAnn,
                    cReturn (cIntLit 0) dummyAnn]

sanityCheckFunc :: a -> Operation a -> [Operation a] -> CTopLevelItem a
sanityCheckFunc dummyAnn scImp implsToCheck =
  cFuncDecl cVoid "sanity_check_impls" [(cPtr cFILE, "df")] $ cBlock argumentBufferDecls (scStatements dummyAnn scImp implsToCheck)
  where
    argumentBufferDecls = scBufferDecls scImp

scBufferDecls :: Operation a -> [(CType, String)]
scBufferDecls scImp = argumentBufferDecls
  where
    args = getOpArguments scImp
    savedBufferDecls = L.map (\(name, tp) -> (toCType tp, name)) args
    refBufferDecls = L.map (\(cType, n) -> (cType, n ++ "_ref")) savedBufferDecls
    testBufferDecls = L.map (\(cType, n) -> (cType, n ++ "_test")) savedBufferDecls
    argumentBufferDecls = savedBufferDecls ++ refBufferDecls ++ testBufferDecls

scStatements :: a -> Operation a -> [Operation a] -> [CStmt a]
scStatements dummyAnn scImp implsToCheck = bufferAllocs ++ refImplSetup ++ refImplTestImplComparisons ++ bufferDeallocs
  where
    bufferAllocs = allocSCBufferStmts dummyAnn scImp
    buffersToDealloc = L.map snd $ scBufferDecls scImp
    bufferDeallocs = L.map (\n -> cExprSt (cFuncall "free" [cVar n]) dummyAnn) buffersToDealloc
    refImplSetup = referenceImplSetup dummyAnn scImp
    refImplTestImplComparisons = []

allocSCBufferStmts :: a -> Operation a -> [CStmt a]
allocSCBufferStmts dummyAnn scImp = allocStmts
  where
    argBufs = getOpArguments scImp
    argBufNames = L.map fst argBufs
    argBufSizes = L.map (\x -> getBufferSize x scImp) argBufNames
    argBufCSizes = L.map (\s -> iExprToCExpr s) argBufSizes
    argBufCDecls = L.map (\(n, tp) -> (getReferencedType $ toCType tp, n)) argBufs
    argBufCDeclsWSize = L.zip argBufCDecls argBufCSizes
    refBufCDeclsWSize = L.map (\((tp, n), sz) -> ((tp, n ++ "_ref"), sz)) argBufCDeclsWSize
    testBufCDeclsWSize = L.map (\((tp, n), sz) -> ((tp, n ++ "_test"), sz)) argBufCDeclsWSize
    allArgBufs = argBufCDeclsWSize ++ refBufCDeclsWSize ++ testBufCDeclsWSize
    allocStmts = L.map (\((tp, n), sz) -> cAssign (cVar n) (cFuncall "malloc" [cMul (cSizeOf tp) sz]) dummyAnn) allArgBufs

timingFunc :: [Operation a] -> CTopLevelItem a
timingFunc [] = error $ "no implementations to time in timingFunc"
timingFunc implsToTime =
  cFuncDecl cVoid "time_impls" [(cPtr cFILE, "df")] $ cBlock [] []

referenceImplSetup :: a -> Operation a -> [CStmt a]
referenceImplSetup dummyAnn scImp = setArgsToRand ++ copyArgsToRefs ++ [callSCImp]
  where
    setArgsToRand = setArgsToRandValues dummyAnn scImp
    copyArgsToRefs = copyArgsToReferences dummyAnn scImp
    callSCImp = cExprSt (cFuncall (getOpName scImp) $ L.map (\(n, tp) -> cVar (n ++ "_ref")) $ getOpArguments scImp) dummyAnn

copyArgsToReferences :: a -> Operation a -> [CStmt a]
copyArgsToReferences dummyAnn scImp = copyArgStmts
  where
    args = getOpArguments scImp
    copyArgStmts = L.map (\(n, tp) -> cExprSt (cFuncall "memcpy" [cVar (n ++ "_ref"), cVar n, cMul (cSizeOf $ toCType tp) (iExprToCExpr $ getBufferSize n scImp)]) dummyAnn) args

setArgsToRandValues :: a -> Operation a -> [CStmt a]
setArgsToRandValues dummyAnn scImp = randValStmts
  where
    args = getOpArguments scImp
    argsWSizes = L.map (\(n, tp) -> ((n, toCType tp), iExprToCExpr $ getBufferSize n scImp)) args
    randValStmts = L.map (setArgToRandValuesCode dummyAnn) argsWSizes

setArgToRandValuesCode :: a -> ((String, CType), CExpr) -> CStmt a
setArgToRandValuesCode dummyAnn ((name, tp), sz) =
  case getReferencedType tp == cDouble of
    True -> cExprSt (cFuncall "rand_doubles" [cVar name, sz]) dummyAnn
    False -> case getReferencedType tp == cFloat of
      True -> cExprSt (cFuncall "rand_floats" [cVar name, sz]) dummyAnn
      False -> error $ "Unrecognized typ ein setArgToRandValuesCode " ++ show tp
    

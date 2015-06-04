module Testing.TestCaseGeneration(genRowAndColMajorExamples, genTestCases) where

import Control.Lens hiding (Const, const)
import Control.Monad
import Control.Monad.Random
import Data.List as L
import Data.Map as M
import Data.Maybe

import Utils.GenerateRandomValues
import IndexExpression
import Utils.MapUtils
import RestrictedLayout
import SymbolTable

dimAndStrideVarsAreSeparable :: [RLayout] -> Bool
dimAndStrideVarsAreSeparable layouts =
  case separateDimAndStrideVars layouts of
    Just _ -> True
    Nothing -> False

separateDimAndStrideVars :: [RLayout] -> Maybe ([Size], [Size])
separateDimAndStrideVars layouts =
  let allLayoutsDimVars = L.concatMap dimensionVars layouts
      allLayoutsStrideVars = L.concatMap strideVars layouts in
  case L.intersect allLayoutsDimVars allLayoutsStrideVars of
    [] -> Just (allLayoutsDimVars, allLayoutsStrideVars)
    _ -> Nothing

allDimsAndStridesAreVars :: [RLayout] -> Bool
allDimsAndStridesAreVars layouts =
  L.and $ L.map isVarSize $ L.concatMap (\l -> dimensions l ++ strides l) layouts

allStridesAreUnique :: [RLayout] -> Bool
allStridesAreUnique layouts =
  let allStrides = L.concatMap strides layouts in
  (L.length $ L.nub allStrides) == L.length allStrides

generalSeparableProblemWithUniqueStrides :: [RLayout] -> Maybe [RLayout]
generalSeparableProblemWithUniqueStrides layouts =
  case dimAndStrideVarsAreSeparable layouts && allDimsAndStridesAreVars layouts && allStridesAreUnique layouts of
    True -> Just layouts
    False -> Nothing

rowStrideToColSizeMap :: [RLayout] -> Map Size Size
rowStrideToColSizeMap layouts =
  let rowStrideColSizePairs = L.map (\l -> (view rrs l, view rnc l)) layouts in
  M.fromList rowStrideColSizePairs

colStrideToRowSizeMap :: [RLayout] -> Map Size Size
colStrideToRowSizeMap layouts =
  let colStrideToRowSizePairs = L.map (\l -> (view rcs l, view rnr l)) layouts in
  M.fromList colStrideToRowSizePairs

assignRandomValuesToDims lo hi layouts =
  let allDimVars = L.concatMap dimensionVars layouts in
  assignRandomValuesInRange lo hi allDimVars

assignRandomValuesToStrides lo hi layouts =
  let allStrideVars = L.concatMap strideVars layouts in
  assignRandomValuesInRange lo hi allStrideVars

assignRandomValuesToDimsAndStrides lo hi layouts =
  let allDimAndStrideVars = L.concatMap (\l -> strideVars l ++ dimensionVars l) layouts in
  assignRandomValuesInRange lo hi allDimAndStrideVars

unitRowStrides layouts =
  let rowStrides = L.map (\l -> view rrs l) layouts in
  M.fromList $ L.zip rowStrides $ L.replicate (length rowStrides) 1

allRowStridesAreUnit layouts =
  let rowStrides = L.map (\l -> view rrs l) layouts in
  L.and $ L.map (\rs -> rs == (con 1)) rowStrides

unitColStrides layouts =
  let colStrides = L.map (\l -> view rcs l) layouts in
  M.fromList $ L.zip colStrides $ L.replicate (length colStrides) 1

generateRowMajorTestCase lo hi layouts = do
  dimVals <- assignRandomValuesToDims lo hi layouts
  let colStrides = unitColStrides layouts
      rowStridesToDims = rowStrideToColSizeMap layouts
      rowStrides = chain rowStridesToDims dimVals in
    return $ M.mapKeys sizeName $ M.union (M.union colStrides rowStrides) dimVals

generateColMajorTestCase lo hi layouts = do
  dimVals <- assignRandomValuesToDims lo hi layouts
  let rowStrides = unitRowStrides layouts
      colStridesToDims = colStrideToRowSizeMap layouts
      colStrides = chain colStridesToDims dimVals in
    return $ M.mapKeys sizeName $ M.union (M.union colStrides rowStrides) dimVals

generateUnitRowStrideCase lo hi layouts =
  case allRowStridesAreUnit layouts of
    True -> do
      dimVals <- assignRandomValuesToDims lo hi layouts
      let colStridesToDims = colStrideToRowSizeMap layouts
          colStrides = chain colStridesToDims dimVals in
        return $ M.mapKeys sizeName $ M.union colStrides dimVals
    False -> return M.empty
  
genRowAndColMajorExamples lo hi st =
  let layouts = genSeparableLayouts st in
      case layouts of
        Just ls -> sequence $ [generateColMajorTestCase lo hi ls,
                               generateRowMajorTestCase lo hi ls]
        Nothing ->
          case mOpSymtabToRLayouts st of
            Just ls -> sequence $ [generateUnitRowStrideCase lo hi ls]
            Nothing -> return []

genSeparableLayouts st = do
  layouts <- mOpSymtabToRLayouts st
  generalSeparableProblemWithUniqueStrides layouts

genVectorVectorExample :: (MonadRandom m, Random a, Num a) => a -> a -> MOpSymtab -> m [Map String a]
genVectorVectorExample lo hi st =
  let layouts = genVectorVectorLayouts st in
  case layouts of
    Just ls -> liftM (\m -> [M.mapKeys sizeName m]) $ assignRandomValuesToDimsAndStrides lo hi ls
    Nothing -> return []
    
genVectorVectorLayouts :: MOpSymtab -> Maybe [RLayout]
genVectorVectorLayouts st = do
  layouts <- mOpSymtabToRLayouts st
  case L.and $ L.map (\l -> isVector l || isScalar l) layouts of
    True -> Just layouts
    False -> Nothing

genTestCases lo hi st = liftM concat $ sequence $ L.map (\f -> f lo hi st) [genVectorVectorExample,
                                                                            genRowAndColMajorExamples]

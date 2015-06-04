module Testing.TestCaseGeneration(genRowAndColMajorCase, genTestCases) where

import Control.Lens hiding (Const, const)
import Control.Monad
import Control.Monad.Random
import Data.List as L
import Data.Map as M
import Data.Maybe

import IndexExpression
import Utils.MapUtils
import RestrictedLayout
import SymbolTable
import Utils.GenerateRandomValues

assignRandomValuesToDims lo hi layouts =
  let allDimVars = L.concatMap dimensionVars layouts in
  assignRandomValuesInRange lo hi allDimVars

assignRandomValuesToStrides lo hi layouts =
  let allStrideVars = L.concatMap strideVars layouts in
  assignRandomValuesInRange lo hi allStrideVars

assignRandomValuesToDimsAndStrides lo hi layouts =
  let allDimAndStrideVars = L.concatMap (\l -> strideVars l ++ dimensionVars l) layouts in
  assignRandomValuesInRange lo hi allDimAndStrideVars

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

generateUnitRowStrideCase lo hi layouts = do
  dimVals <- assignRandomValuesToDims lo hi layouts
  let colStridesToDims = colStrideToRowSizeMap layouts
      colStrides = chain colStridesToDims dimVals in
    return $ M.mapKeys sizeName $ M.union colStrides dimVals

generateUnitColStrideCase lo hi layouts = do
  dimVals <- assignRandomValuesToDims lo hi layouts
  let rowStridesToDims = rowStrideToColSizeMap layouts
      rowStrides = chain rowStridesToDims dimVals in
    return $ M.mapKeys sizeName $ M.union rowStrides dimVals

genRowAndColMajorCase lo hi st =
  let layouts = genSeparableLayouts st in
      case layouts of
        Just ls -> sequence $ [generateColMajorTestCase lo hi ls,
                               generateRowMajorTestCase lo hi ls]
        Nothing -> return []

genUnitRowStrideCase lo hi st = 
  case mOpSymtabToRLayouts st of
    Just ls -> case allRowStridesAreUnit ls && someColStridesAreNonUnit ls of
      True -> sequence $ [generateUnitRowStrideCase lo hi ls]
      False -> return []
    Nothing -> return []

genUnitColStrideCase lo hi st = 
  case mOpSymtabToRLayouts st of
    Just ls -> case allColStridesAreUnit ls && someRowStridesAreNonUnit ls of
      True -> sequence $ [generateUnitColStrideCase lo hi ls]
      False -> return []
    Nothing -> return []

genSeparableLayouts st = do
  layouts <- mOpSymtabToRLayouts st
  generalSeparableProblemWithUniqueStrides layouts

genVectorVectorCase :: (MonadRandom m, Random a, Num a) => a -> a -> MOpSymtab -> m [Map String a]
genVectorVectorCase lo hi st =
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

genTestCases lo hi st = liftM concat $ sequence $ L.map (\f -> f lo hi st) [genVectorVectorCase,
                                                                            genRowAndColMajorCase,
                                                                            genUnitRowStrideCase,
                                                                            genUnitColStrideCase]

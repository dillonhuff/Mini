{-# LANGUAGE TemplateHaskell #-}

module Testing.RestrictedLayout(RLayout,
                        rLayout,
                        rnr, rnc, rrs, rcs,
                        dimensionVars, strideVars,
                        strides, dimensions,
                        isVector, isRowVector, isColVector, isScalar, isMatrix,
                        mOpSymtabToRLayouts,
                        Size,
                        var, con,
                        sizeName, isVarSize,
                        rowStrideToColSizeMap, colStrideToRowSizeMap,
                        generalSeparableProblemWithUniqueStrides,
                        unitColStrides, unitRowStrides,
                        allRowStridesAreUnit, allColStridesAreUnit,
                        someRowStridesAreNonUnit, someColStridesAreNonUnit,
                        genVectorVectorLayouts) where

import Control.Lens hiding (Const, const)
import Control.Lens.TH
import Data.List as L
import Data.Map as M

import Core.IndexExpression
import Core.SymbolTable

data Size
  = Const Int
  | Var String
    deriving (Eq, Ord, Show)

con i = Const i
var i = Var i

isVarSize (Var _) = True
isVarSize _ = False

sizeName (Var n) = n

nonUnit (Const i) = i /= 1
nonUnit _ = True

data RLayout
  = RLayout {
    _rnr :: Size,
    _rnc :: Size,
    _rrs :: Size,
    _rcs :: Size
    } deriving (Eq, Ord, Show)

makeLenses ''RLayout

rLayout numRows numCols rowStride colStride =
  RLayout numRows numCols rowStride colStride

dimensions l = [view rnr l, view rnc l]
strides l = [view rrs l, view rcs l]

dimensionVars l = L.filter isVarSize $ dimensions l
strideVars l = L.filter isVarSize $ strides l

isVector l = isRowVector l || isColVector l
isRowVector l = view rnr l == con 1 && (nonUnit $ view rnc l)
isColVector l = view rnc l == con 1 && (nonUnit $ view rnr l)
isScalar l = view rnr l == con 1 && view rnc l == con 1
isMatrix l = nonUnit (view rnr l) && nonUnit (view rnc l)


ieToSize :: IExpr -> Maybe Size
ieToSize ie =
  case isConst ie of
    True -> Just $ con $ constVal ie
    False -> case isVar ie of
      True -> Just $ var $ varName ie
      False -> Nothing

layoutToRLayout :: Layout -> Maybe RLayout
layoutToRLayout l = do
  r <- ieToSize $ view nr l
  c <- ieToSize $ view nc l
  rStride <- ieToSize $ view rs l
  cStride <- ieToSize $ view cs l
  return $ rLayout r c rStride cStride

mOpSymtabToRLayouts :: MOpSymtab -> Maybe [RLayout]
mOpSymtabToRLayouts symTab =
  let layouts = allLayouts symTab in
  sequence $ L.map layoutToRLayout layouts

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
  let rowStrideColSizePairs = L.map (\l -> (view rrs l, view rnc l)) layouts
      rowStrideVarColSizePairs = L.filter (\(cs, nr) -> isVarSize cs) rowStrideColSizePairs in
  M.fromList rowStrideVarColSizePairs

colStrideToRowSizeMap :: [RLayout] -> Map Size Size
colStrideToRowSizeMap layouts =
  let colStrideToRowSizePairs = L.map (\l -> (view rcs l, view rnr l)) layouts
      colStrideVarToRowSizePairs = L.filter (\(cs, nr) -> isVarSize cs) colStrideToRowSizePairs in
  M.fromList colStrideVarToRowSizePairs

unitRowStrides layouts =
  let rowStrides = L.map (\l -> view rrs l) layouts in
  M.fromList $ L.zip rowStrides $ L.replicate (length rowStrides) 1

allRowStridesAreUnit layouts =
  let rowStrides = L.map (\l -> view rrs l) layouts in
  L.and $ L.map (\rs -> rs == (con 1)) rowStrides

someColStridesAreNonUnit layouts =
  let colStrides = L.map (\l -> view rcs l) layouts in
  L.or $ L.map (\cs -> cs /= (con 1)) colStrides

allColStridesAreUnit layouts =
  let colStrides = L.map (\l -> view rcs l) layouts in
  L.and $ L.map (\rs -> rs == (con 1)) colStrides

someRowStridesAreNonUnit layouts =
  let rowStrides = L.map (\l -> view rrs l) layouts in
  L.or $ L.map (\cs -> cs /= (con 1)) rowStrides

unitColStrides layouts =
  let colStrides = L.map (\l -> view rcs l) layouts in
  M.fromList $ L.zip colStrides $ L.replicate (length colStrides) 1

genVectorVectorLayouts :: MOpSymtab -> Maybe [RLayout]
genVectorVectorLayouts st = do
  layouts <- mOpSymtabToRLayouts st
  case L.and $ L.map (\l -> isVector l || isScalar l) layouts of
    True -> Just layouts
    False -> Nothing

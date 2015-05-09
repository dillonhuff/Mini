\documentclass{article}

%include lhs2TeX.fmt

\begin{document}

\title{Test Case Generator}
\author{Dillon Huff}
\maketitle

\section {Purpose}

When a matrix computation is described in Mini
each matrix (or vector or scalar) involved must
have a number of rows, a number of columns and a
row and column stride. For some operations all of
these may be fixed values, but for most purposes the
user will want to generate functions that take dimensions
and strides as parameters.

Timing and sanity checking code where all dimensions
and strides are fixed is simple. Print the final code
to a file, generate the test harness and compile and
run the resulting program. However, when problems contain
dimension or stride parameters that are variable (they are
arguments to the function implementing the specified operation)
the Mini compiler must choose concrete values of dimensions and
strides to use as parameters to the operation function during
testing.

This module provides facilities for generating legal values for
dimensions and strides from a symbol table for an operation.

\section{Difficulties}

This may seem like an easy problem. A naive approach would be
to generate a test case by picking random natural numbers for
each stride and dimension. The problem with this approach is
that not all dimension-stride combinations are legal. For
example a 10 by 10 matrix can have a row stride of 1 and a
column stride of 10, but it cannot have a row stride of 1 and
a column stride of 1.

Dealing with the problem of generating legal stride and dimension
combinations in general is tricky, but in practice only a few cases
really matter. This module makes the following assumptions:

\begin{itemize}

\item \textbf{simple lengths:} All dimension and stride lengths are either constant values e.g. $12$
or single variables e.g. $n$. There are no dimensions or strides with lengths like $2*n + 3$

\item \textbf{dimension-stride separation:} Any variable that is used as a dimension is not used as a stride
and vice versa

\end{itemize}

\section{Module front matter}

This module uses TemplateHaskell to generate lenses
for record data structures.

\begin{code}
{-# LANGUAGE TemplateHaskell #-}
module TestCaseGeneration() where

import Control.Lens hiding (Const, const)
import Control.Lens.TH
import Control.Monad.Random
import Data.List as L
import Data.Map as M
import Data.Maybe

import GenerateRandomValues
import IndexExpression
import MapUtils
import SymbolTable

\end{code}

\section{Core data structures}

The Size ADT represents the size of a dimension or stride.

\begin{code}

data Size
  = Const Int
  | Var String
    deriving (Eq, Ord, Show)

con i = Const i
var i = Var i

isVarSize (Var _) = True
isVarSize _ = False

\end{code}

The RLayout data structure represents the format of a matrix.
It is analagous to the Layout data structure, but it restricts
strides and dimensions to be either vars or constants, not any
form allowed by IExpr.

\begin{code}

data RLayout
  = RLayout {
    _rnr :: Size,
    _rnc :: Size,
    _rrs :: Size,
    _rcs :: Size
    } deriving (Eq, Ord, Show)

makeLenses ''RLayout

rLayout numRows numCols rowStride colStride = RLayout numRows numCols rowStride colStride

dimensions l = [view rnr l, view rnc l]
strides l = [view rrs l, view rcs l]

dimensionVars l = L.filter isVarSize $ dimensions l
strideVars l = L.filter isVarSize $ strides l
\end{code}

\section{Conversion operations}

The following code converts normal Layouts stored in
the symtab into restricted form or fails if the conversion
cannot be done.

\begin{code}

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

\end{code}

\section{Check compliance with special cases}

The following functions check whether a group of
RLayouts fits one of the forms that the system can
generate test cases for.

\begin{code}

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

\end{code}

\section{Test case generation for problems with general, separable sizes and unique strides}

When dimensions and strides are all variables, dimension and stride variables
are separable, and all stride variables are unique it is possible to assign
legal dimension and stride values by picking random natural numbers (not including
zero) for dimensions and then for each matrix assigning the row
stride for that matrix to be the number of columns or the columns stride to be the
number of rows and then leaving the other stride as 1. In order to do this we need
to have a map from the stride variables involved in a problem to the size variables
that determine them.

\begin{code}

rowStrideToColSizeMap :: [RLayout] -> Map Size Size
rowStrideToColSizeMap layouts =
  let rowStrideColSizePairs = L.map (\l -> (view rrs l, view rnc l)) layouts in
  M.fromList rowStrideColSizePairs

colStrideToRowSizeMap :: [RLayout] -> Map Size Size
colStrideToRowSizeMap layouts =
  let colStrideToRowSizePairs = L.map (\l -> (view rcs l, view rnr l)) layouts in
  M.fromList colStrideToRowSizePairs

\end{code}

Dimensions are given random values in a specified range.

\begin{code}

assignRandomValuesToDims lo hi layouts =
  let allDimVars = L.concatMap dimensionVars layouts in
  assignRandomValuesInRange lo hi allDimVars

\end{code}

Depending on whether the matrices are supposed to be in row or column major order
either all column strides are set to 1 or all row strides are set to 1.

\begin{code}

unitRowStrides layouts =
  let rowStrides = L.map (\l -> view rrs l) layouts in
  M.fromList $ L.zip rowStrides $ L.replicate (length rowStrides) 1

unitColStrides layouts =
  let colStrides = L.map (\l -> view rcs l) layouts in
  M.fromList $ L.zip colStrides $ L.replicate (length colStrides) 1

\end{code}

Creating row major or column major example dimensions and strides is a matter
of separating row strides, column strides and dimensions and then assigning
the appropriate stride to be ones, and the dimensions to be random values in a range.
The other stride's values are determined by the dimension values.

\begin{code}

generateRowMajorTestCase lo hi layouts = do
  dimVals <- assignRandomValuesToDims lo hi layouts
  let colStrides = unitColStrides layouts
      rowStridesToDims = rowStrideToColSizeMap layouts
      rowStrides = chain rowStridesToDims dimVals in
    return $ M.union (M.union colStrides rowStrides) dimVals

generateColMajorTestCase lo hi layouts = do
  dimVals <- assignRandomValuesToDims lo hi layouts
  let rowStrides = unitRowStrides layouts
      colStridesToDims = colStrideToRowSizeMap layouts
      colStrides = chain colStridesToDims dimVals in
    return $ M.union (M.union colStrides rowStrides) dimVals

\end{code}

In practice the module needs to take in a symbol table, convert its entries into
the restricted layout format, check that the dimensions
and strides are general and separable and that strides are unique and then generate
a test case. In addition we want the final test case to simply be a map from String
to Int so that other modules don't have to manipulate the restricted layout.

\begin{code}

genRowAndColMajorExamples lo hi st =
  let layouts = genSeparableLayouts st in
      case layouts of
        Just ls -> sequence $ [generateColMajorTestCase lo hi ls, generateRowMajorTestCase lo hi ls]
        Nothing -> return []
  
genSeparableLayouts st = do
  layouts <- mOpSymtabToRLayouts st
  generalSeparableProblemWithUniqueStrides layouts

\end{code}

\end{document}

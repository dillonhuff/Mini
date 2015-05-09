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
import Data.List as L
import Data.Map as M

import IndexExpression
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

\end{code}

\section{Stride to dimension map construction}

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

\end{document}

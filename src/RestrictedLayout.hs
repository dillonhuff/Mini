{-# LANGUAGE TemplateHaskell #-}

module RestrictedLayout(RLayout,
                        rnr, rnc, rrs, rcs,
                        dimensionVars, strideVars,
                        strides, dimensions,
                        mOpSymtabToRLayouts,
                        Size,
                        sizeName, isVarSize) where

import Control.Lens hiding (Const, const)
import Control.Lens.TH
import Data.List as L

import IndexExpression
import SymbolTable

data Size
  = Const Int
  | Var String
    deriving (Eq, Ord, Show)

con i = Const i
var i = Var i

isVarSize (Var _) = True
isVarSize _ = False

sizeName (Var n) = n

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

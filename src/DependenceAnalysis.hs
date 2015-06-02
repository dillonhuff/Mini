module DependenceAnalysis(isFlowDependent,
                          indexRange) where

import Data.List as L

import IndexExpression
import Syntax

isFlowDependent :: (Show a) => [IndexRange] -> Statement a -> Statement a -> Bool
isFlowDependent iRanges s1 s2 =
  0 < (L.length $ L.intersectBy (operandsEqual iRanges) [operandWritten s1] (operandsRead s2))

operandsEqual :: [IndexRange] -> Operand -> Operand -> Bool
operandsEqual iRanges l r = case l == r of
  True -> True
  False -> case isBufferVal l && isBufferVal r of
    True -> buffersCouldBeEqual iRanges l r
    False -> False

buffersCouldBeEqual iRanges l r =
  bufferName l == bufferName r

data IndexRange
  = IndexRange IExpr IExpr
    deriving (Eq, Ord, Show)

indexRange start end = IndexRange start end

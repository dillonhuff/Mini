
module MiniOperation(Operation,
                     operation,
                     getOpName, getOpArguments, getBufferSize,
                     toCFunc,
                     Optimization,
                     optimizationName, optimizationFunction,
                     optimization) where

import Data.List as L

import CGen
import IndexExpression
import Syntax

data Operation a
  = Operation String MiniSymtab (Block a)
    deriving (Eq, Ord, Show)

applyToOpBlock :: (Block a -> Block a) -> Operation a -> Operation a
applyToOpBlock f (Operation n st b) = Operation n st (f b)

toCFunc :: Operation a -> CTopLevelItem a
toCFunc op = cFuncDecl cVoid (getOpName op) cArgs cBlock
  where
    cArgs = L.map (\(n, tp) -> (toCType tp, n)) $ getOpArguments op
    cBlock = toCBlock (getOpLocalVars op) (getOpBlock op)

operation = Operation

getOpName (Operation n _ _) = n
getOpArguments (Operation _ st _) = arguments st
getOpBlock (Operation _ _ b) = b
getOpLocalVars (Operation _ st _) = localVars st

getBufferSize :: String -> Operation a -> IExpr
getBufferSize _ (Operation _ _ _) = iConst 1


data Optimization a
  = Optimization {
    optimizationName :: String,
    optimizationFunction :: Operation a -> Operation a
    }

optimization n f = Optimization n f

applyOptimization optimization operation =
  (optimizationFunction optimization) operation

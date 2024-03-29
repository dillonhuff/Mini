
module Core.MiniOperation(Operation,
                        operation,
                        getOpName, getBufferSize, getOptimizationsApplied,
                        getOpLocalVars,
                        getMiniOpSymtab, getOpBlock, makeOperation,
                        getIndexArgs, getBufferArgs, getOpArguments,
                        applyToOpBlock, allNonLoopStatementsInOperation,
                        Optimization,
                        optimizationName, optimizationFunction,
                        applyOptimization,
                        optimization, sequenceOptimization) where

import Data.List as L

import BackEnd.CGen
import Core.IndexExpression
import Core.SymbolTable hiding (getBufferSize)
import Core.MiniSyntax
import Core.Type

data Operation a
  = Operation String [Optimization a] MiniSymtab (Block a)

instance Eq a => Eq (Operation a) where
  (==) (Operation n1 _ mst1 blk1) (Operation n2 _ mst2 blk2) = n1 == n2 && mst1 == mst2 && blk1 == blk2

instance Ord a => Ord (Operation a) where
  compare (Operation n1 _ _ _) (Operation n2 _ _ _) = compare n1 n2

instance Show (Operation a) where
  show (Operation n _ _ _) = n

addOptimization :: Optimization a -> Operation a -> Operation a
addOptimization opt (Operation n opts mst blk) = Operation (n ++ optimizationName opt) (opt:opts) mst blk

applyToOpBlock :: (Block a -> Block a) -> Operation a -> Operation a
applyToOpBlock f (Operation n opts st b) = Operation n opts st (f b)

allNonLoopStatementsInOperation (Operation _ _ _ b) =
  L.concatMap nonLoopStatements $ blockStatements b

operation n st blk = Operation n [] st blk

makeOperation n ops st blk = Operation n ops st blk

getMiniOpSymtab (Operation _ _ st _) = st
getOpName (Operation n _ _ _) = n
getOpArguments (Operation _ _ st _) = arguments st
getIndexArgs op = L.filter (\(n, tp) -> isIndex tp) $ getOpArguments op
getBufferArgs op = L.filter (\(n, tp) -> isBuffer tp) $ getOpArguments op
getOptimizationsApplied (Operation _ opts _ _) = opts
getOpBlock (Operation _ _ _ b) = b
getOpLocalVars (Operation _ _ st _) = localVars st

getBufferSize :: String -> Operation a -> IExpr
getBufferSize n (Operation _ _ st _) = getMiniSymInfo n bufferSize st

data Optimization a
  = Optimization {
    optimizationName :: String,
    optimizationFunction :: Operation a -> Operation a
    }

optimization n f = Optimization n f

applyOptimization optimization operation =
  addOptimization optimization $ (optimizationFunction optimization) operation

instance Show (Optimization a) where
  show opt = optimizationName opt

sequenceOptimization :: String -> [Optimization a] -> Optimization a
sequenceOptimization name opts =
  optimization name $ applyOptimizationSequence opts

applyOptimizationSequence :: [Optimization a] -> Operation a -> Operation a
applyOptimizationSequence [] op = op
applyOptimizationSequence (optimization:rest) op =
  applyOptimization optimization $ applyOptimizationSequence rest op

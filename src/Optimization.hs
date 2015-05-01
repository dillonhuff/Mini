module Optimization(Optimization,
                    optimizationName, optimizationFunction,
                    optimization) where

import Syntax

data Optimization a
  = Optimization {
    optimizationName :: String,
    optimizationFunction :: Operation a -> Operation a
    }

optimization n f = Optimization n f

applyOptimization optimization operation =
  (optimizationFunction optimization) operation

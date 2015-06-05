module Optimizations.IndexExpressionOptimizations(evalIExprConstants) where

import IndexExpression
import MiniOperation
import Syntax

evalIExprConstants =
  optimization
        "EvaluateIExprConstants"
        (applyToOpBlock (transformBlock (transformStatementIExprs evaluateIExprConstants)))


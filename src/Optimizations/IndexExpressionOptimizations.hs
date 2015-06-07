module Optimizations.IndexExpressionOptimizations(evalIExprConstants) where

import IndexExpression
import MiniOperation
import MiniSyntax

evalIExprConstants =
  optimization
        "EvaluateIExprConstants"
        (applyToOpBlock (transformBlock (transformStatementIExprs evaluateIExprConstants)))


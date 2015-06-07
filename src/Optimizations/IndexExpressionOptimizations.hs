module Optimizations.IndexExpressionOptimizations(evalIExprConstants) where

import IndexExpression
import Core.MiniOperation
import Core.MiniSyntax

evalIExprConstants =
  optimization
        "EvaluateIExprConstants"
        (applyToOpBlock (transformBlock (transformStatementIExprs evaluateIExprConstants)))


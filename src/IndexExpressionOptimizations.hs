module IndexExpressionOptimizations(EvalIExprConsts) where

import IndexExpression
import Optimization
import Syntax

data EvalIExprConsts
  = EvalIExprConsts
    deriving (Eq, Ord, Show)

evalIExprConsts =
  optimization
        "EvaluateIExprConstants"
        (applyToOpBlock (transformBlock (transformStatementIExprs evaluateIExprConstants)))


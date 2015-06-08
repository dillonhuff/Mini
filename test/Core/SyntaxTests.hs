module Core.SyntaxTests(allSyntaxTests) where

import Core.IndexExpression
import Core.MiniSyntax
import TestUtils

allSyntaxTests = do
  testFunction (transformBlock (transformStatementIExprs evaluateIExprConstants)) evalIExprConstCases

evalIExprConstCases :: [(Block String, Block String)]
evalIExprConstCases =
  [(block [], block []),
   (block [for "i" (iConst 2) (iMul (iConst 2) (iConst 9)) (iVar "n") (block []) ""],
    block [for "i" (iConst 2) (iConst 18) (iVar "n") (block []) ""])]

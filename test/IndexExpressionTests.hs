module IndexExpressionTests() where

import IndexExpression
import TestUtils

allIndexExpressionTests = do
  testFunction evaluateIExprConstants evalConstTests

evalConstTests =
  [(iVar "n", iVar "n"),
   (iConst 4, iConst 4),
   (iMul (iConst 2) (iConst 3), iConst 6),
   (iAdd (iConst 9) (iConst (-3)), iConst 6),
   (iAdd (iConst 4) (iMul (iConst 3) (iConst (-8))), iConst (-20))]

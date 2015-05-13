module IndexExpressionTests(allIndexExpressionTests) where

import Data.List as L

import IndexExpression
import TestUtils

allIndexExpressionTests = do
  testFunction evaluateIExprConstants evalConstTests
  testFunction ieToConst successIEConstCases
  testFunction ieToConst failIEConstCases

evalConstTests =
  [(iVar "n", iVar "n"),
   (iConst 4, iConst 4),
   (iMul (iConst 2) (iConst 3), iConst 6),
   (iAdd (iConst 9) (iConst (-3)), iConst 6),
   (iAdd (iConst 4) (iMul (iConst 3) (iConst (-8))), iConst (-20))]

successIEConstCases =
  L.map (\(x, y) -> (x, Just y))
  [(iConst 3, 3),
   (iMul (iConst 2) (iConst (-3)), -6),
   (iAdd (iConst 9) (iConst (-3)), 6),
   (iAdd (iConst 4) (iMul (iConst 3) (iConst (-8))), (-20))]

failIEConstCases =
  L.map (\x -> (x, Nothing))
  [iVar "x",
   iMul (iConst 2) (iVar "3"),
   iAdd (iVar "4") (iConst 3)]

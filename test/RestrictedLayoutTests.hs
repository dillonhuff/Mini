module RestrictedLayoutTests(allRestrictedLayoutTests) where

import Testing.RestrictedLayout
import TestUtils

allRestrictedLayoutTests = do
  testFunction isScalar isScalarCases
  testFunction isRowVector isRowVectorCases
  testFunction isColVector isColVectorCases
  testFunction isVector isVectorCases
  testFunction isMatrix isMatrixCases

isScalarCases =
  [(matrix1, False),
   (matrix2, False),
   (matrix3, False),
   (rowVec1, False),
   (rowVec2, False),
   (colVec1, False),
   (colVec2, False),
   (scalar, True)]

isRowVectorCases =
  [(matrix1, False),
   (matrix2, False),
   (matrix3, False),
   (rowVec1, True),
   (rowVec2, True),
   (colVec1, False),
   (colVec2, False),
   (scalar, False)]

isColVectorCases =
  [(matrix1, False),
   (matrix2, False),
   (matrix3, False),
   (rowVec1, False),
   (rowVec2, False),
   (colVec1, True),
   (colVec2, True),
   (scalar, False)]

isVectorCases =
  [(matrix1, False),
   (matrix2, False),
   (matrix3, False),
   (rowVec1, True),
   (rowVec2, True),
   (colVec1, True),
   (colVec2, True),
   (scalar, False)]

isMatrixCases =
  [(matrix1, True),
   (matrix2, True),
   (matrix3, True),
   (rowVec1, False),
   (rowVec2, False),
   (colVec1, False),
   (colVec2, False),
   (scalar, False)]


matrix1 = rLayoutS (var "i") (var "j")
matrix2 = rLayoutS (var "i") (con 12)
matrix3 = rLayoutS (con 2) (var "j")
rowVec1 = rLayoutS (con 1) (var "j")
rowVec2 = rLayoutS (con 1) (con 2)
colVec1 = rLayoutS (var "i") (con 1)
colVec2 = rLayoutS (con 34) (con 1)
scalar = rLayoutS (con 1) (con 1)

rLayoutS r c = rLayout r c (var "i") (var "j")

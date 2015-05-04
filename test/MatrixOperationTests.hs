module MatrixOperationTests() where

import IndexExpression
import MatrixOperation
import MOpSyntax
import SymbolTable
import TestUtils
import Token

allMatrixOperationTests = do
  testFunction matrixOperationToMOp matOpToMOpCases

matOpToMOpCases =
  [(dMatrixOperation "nothing" [] [], mOp "nothing" (mOpSymtab []) []),
   (dMatrixOperation "arg" [("A", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124)))] [],
    mOp "arg" (mOpSymtab [("A", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124)))]) []),
   (dMatrixOperation "oneOp" [("A", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124)))] [dMatAsg "c" (dMatrixSub (dMatName "a") (dMatName "b"))],
    mOp "oneOp" (mOpSymtab [("A", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124)))]) [])]

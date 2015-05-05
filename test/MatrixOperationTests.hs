module MatrixOperationTests(allMatrixOperationTests) where

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
   (dMatrixOperation "oneOp" [("A", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124))),
                              ("B", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124))),
                              ("C", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124)))]
                             [dMatAsg "C" (dMatrixSub (dMatName "A") (dMatName "B"))],
    mOp "oneOp" (mOpSymtab [("A", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124))),
                            ("B", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124))),
                            ("C", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124))),
                            ("tmp0", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124)))])
                            [msub "A" "B" "tmp0",
                             masg "tmp0" "C"])]

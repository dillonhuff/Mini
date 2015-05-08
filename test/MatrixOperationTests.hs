module MatrixOperationTests(allMatrixOperationTests) where

import IndexExpression
import MatrixOperation
import MOpSyntax
import SymbolTable
import TestUtils
import Token

allMatrixOperationTests = do
  testFunction matrixOperationToMOp matOpToMOpCases
  testFunction (\e -> simplifySymtab e simpleSt) simpleStCases
  testFunction (\stmt -> typeCheckStmt stmt simpleSt) simpleStmtCases

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
                             masg "tmp0" "C"]),
   (dMatrixOperation "oneOp" [("A", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124))),
                              ("B", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124))),
                              ("C", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124)))]
                             [dMatAsg "C" (dMatrixAdd (dMatName "A") (dMatName "B"))],
    mOp "oneOp" (mOpSymtab [("A", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124))),
                            ("B", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124))),
                            ("C", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124))),
                            ("tmp0", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124)))])
                            [madd "A" "B" "tmp0",
                             masg "tmp0" "C"])]

simpleStCases =
  [(dMatName "A", simpleSt),
   (dMatrixAdd (dMatName "A") (dMatName "B"), addSt),
   (dMatrixSub (dMatName "A") (dMatName "B"), addSt),
   (dMatrixMul (dMatName "A") (dMatName "B"), mulSt),
   (dScalarMul (dMatName "alpha") (dMatName "B"), simpleSt),
   (dMatrixTrans (dMatName "A"), simpleSt)]

mulSt =
  mOpSymtab [("A", mOpSymInfo arg singleFloat $ layout (iVar "k") (iVar "l") (iVar "ars") (iVar "acs")),
             ("B", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "brs") (iVar "bcs")),
             ("alpha", mOpSymInfo arg singleFloat $ layout (iConst 1) (iConst 1) (iConst 1) (iConst 1))]

addSt =
  mOpSymtab [("A", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "ars") (iVar "acs")),
             ("B", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "brs") (iVar "bcs")),
             ("alpha", mOpSymInfo arg singleFloat $ layout (iConst 1) (iConst 1) (iConst 1) (iConst 1))]             

simpleSt =
  mOpSymtab [("A", mOpSymInfo arg singleFloat $ layout (iVar "k") (iVar "j") (iVar "ars") (iVar "acs")),
             ("B", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "brs") (iVar "bcs")),
             ("alpha", mOpSymInfo arg singleFloat $ layout (iConst 1) (iConst 1) (iConst 1) (iConst 1))]

simpleStmtCases =
  [(dMatAsg "A" (dMatName "B"), simpleSt),
   (dMatAsg "T" (dMatName "A"), asgSt),
   (dMatAsg "T" (dMatrixMul (dMatName "A") (dMatName "B")), asgMulSt)]

asgSt =
  mOpSymtab [("A", mOpSymInfo arg singleFloat $ layout (iVar "k") (iVar "j") (iVar "ars") (iVar "acs")),
             ("B", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "brs") (iVar "bcs")),
             ("alpha", mOpSymInfo arg singleFloat $ layout (iConst 1) (iConst 1) (iConst 1) (iConst 1)),
             ("T", mOpSymInfo local singleFloat $ layout (iVar "k") (iVar "j") (iVar "ars") (iVar "acs"))]

asgMulSt =
  mOpSymtab [("A", mOpSymInfo arg singleFloat $ layout (iVar "k") (iVar "l") (iVar "ars") (iVar "acs")),
             ("B", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "brs") (iVar "bcs")),
             ("alpha", mOpSymInfo arg singleFloat $ layout (iConst 1) (iConst 1) (iConst 1) (iConst 1)),
             ("T", mOpSymInfo local singleFloat $ layout (iVar "k") (iVar "m") (iVar "ars") (iVar "acs"))]

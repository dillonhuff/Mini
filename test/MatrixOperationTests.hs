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
  testFunction (\stmt -> typeCheckStmt stmt complexSt) complexStmtCases

matOpToMOpCases =
  [(dMatrixOperation "nothing" [] [], mOp "nothing" (mOpSymtab []) []),
   (dMatrixOperation "arg" [("A", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124)))] [],
    mOp "arg" (mOpSymtab [("A", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124)))]) []),
   (dMatrixOperation "oneOp" symList [dMatAsg "C" (dMatrixSub (dMatName "A") (dMatName "B"))],
    mOp "oneOp" allConstSt [msub "A" "B" "tmp0", masg "tmp0" "C"]),
   (dMatrixOperation "oneOp" symList [dMatAsg "C" (dMatrixAdd (dMatName "A") (dMatName "B"))],
    mOp "oneOp" allConstSt [madd "A" "B" "tmp0", masg "tmp0" "C"])]

allConstSt =
  mOpSymtab symList

symList = [("A", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124))),
           ("B", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124))),
           ("C", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124))),
           ("tmp0", mOpSymInfo arg singleFloat (layout (iConst 17) (iConst 17) (iConst 1) (iConst 124)))]
  
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
             ("C", mOpSymInfo arg singleFloat $ layout (iVar "p") (iVar "q") (iVar "crs") (iVar "ccs")),             
             ("alpha", mOpSymInfo arg singleFloat $ layout (iConst 1) (iConst 1) (iConst 1) (iConst 1))]

addSt =
  mOpSymtab [("A", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "ars") (iVar "acs")),
             ("B", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "brs") (iVar "bcs")),
             ("C", mOpSymInfo arg singleFloat $ layout (iVar "p") (iVar "q") (iVar "crs") (iVar "ccs")),             
             ("alpha", mOpSymInfo arg singleFloat $ layout (iConst 1) (iConst 1) (iConst 1) (iConst 1))]             

simpleSt =
  mOpSymtab [("A", mOpSymInfo arg singleFloat $ layout (iVar "k") (iVar "j") (iVar "ars") (iVar "acs")),
             ("B", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "brs") (iVar "bcs")),
             ("C", mOpSymInfo arg singleFloat $ layout (iVar "p") (iVar "q") (iVar "crs") (iVar "ccs")),
             ("alpha", mOpSymInfo arg singleFloat $ layout (iConst 1) (iConst 1) (iConst 1) (iConst 1))]

simpleStmtCases =
  [(dMatAsg "A" (dMatName "B"), aEqBSt),
   (dMatAsg "T" (dMatName "A"), asgSt),
   (dMatAsg "T" (dMatrixMul (dMatName "A") (dMatName "B")), asgMulSt),
   (dMatAsg "A" (dMatrixAdd (dMatName "A") (dMatName "B")), asgAddSt),
   (dMatAsg "C" (dMatrixAdd (dMatName "A") (dMatName "B")), asgAddAssignSt),
   (dMatAsg "C" (dMatrixMul (dMatName "A") (dMatName "B")), asgMulAssignSt)]

aEqBSt =
  mOpSymtab  [("A", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "ars") (iVar "acs")),
             ("B", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "brs") (iVar "bcs")),
             ("C", mOpSymInfo arg singleFloat $ layout (iVar "p") (iVar "q") (iVar "crs") (iVar "ccs")),
             ("alpha", mOpSymInfo arg singleFloat $ layout (iConst 1) (iConst 1) (iConst 1) (iConst 1))]

asgSt =
  mOpSymtab [("A", mOpSymInfo arg singleFloat $ layout (iVar "k") (iVar "j") (iVar "ars") (iVar "acs")),
             ("B", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "brs") (iVar "bcs")),
             ("C", mOpSymInfo arg singleFloat $ layout (iVar "p") (iVar "q") (iVar "crs") (iVar "ccs")),
             ("alpha", mOpSymInfo arg singleFloat $ layout (iConst 1) (iConst 1) (iConst 1) (iConst 1)),
             ("T", mOpSymInfo local singleFloat $ layout (iVar "k") (iVar "j") (iVar "ars") (iVar "acs"))]

asgMulSt =
  mOpSymtab [("A", mOpSymInfo arg singleFloat $ layout (iVar "k") (iVar "l") (iVar "ars") (iVar "acs")),
             ("B", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "brs") (iVar "bcs")),
             ("C", mOpSymInfo arg singleFloat $ layout (iVar "p") (iVar "q") (iVar "crs") (iVar "ccs")),             
             ("alpha", mOpSymInfo arg singleFloat $ layout (iConst 1) (iConst 1) (iConst 1) (iConst 1)),
             ("T", mOpSymInfo local singleFloat $ layout (iVar "k") (iVar "m") (iVar "ars") (iVar "acs"))]

asgAddSt =
  mOpSymtab [("A", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "ars") (iVar "acs")),
             ("B", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "brs") (iVar "bcs")),
             ("C", mOpSymInfo arg singleFloat $ layout (iVar "p") (iVar "q") (iVar "crs") (iVar "ccs")),             
             ("alpha", mOpSymInfo arg singleFloat $ layout (iConst 1) (iConst 1) (iConst 1) (iConst 1))]

asgAddAssignSt =
  mOpSymtab [("A", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "ars") (iVar "acs")),
             ("B", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "brs") (iVar "bcs")),
             ("C", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "crs") (iVar "ccs")),             
             ("alpha", mOpSymInfo arg singleFloat $ layout (iConst 1) (iConst 1) (iConst 1) (iConst 1))]

asgMulAssignSt =
  mOpSymtab [("A", mOpSymInfo arg singleFloat $ layout (iVar "k") (iVar "l") (iVar "ars") (iVar "acs")),
             ("B", mOpSymInfo arg singleFloat $ layout (iVar "l") (iVar "m") (iVar "brs") (iVar "bcs")),
             ("C", mOpSymInfo arg singleFloat $ layout (iVar "k") (iVar "m") (iVar "crs") (iVar "ccs")),
             ("alpha", mOpSymInfo arg singleFloat $ layout (iConst 1) (iConst 1) (iConst 1) (iConst 1))]

complexStmtCases =
  [(dMatAsg "A" (dMatrixMul (dMatName "P") (dMatName "Q")), dupCondSt)]

complexSt =
  mOpSymtab [("A", dSInf "A" (iVar "a") (iVar "b")),
             ("P", dSInf "P" (iVar "c") (iVar "b")),
             ("Q", dSInf "Q" (iVar "b") (iVar "a"))]

dupCondSt =
  mOpSymtab [("A", dSInf "A" (iVar "c") (iVar "c")),
             ("P", dSInf "P" (iVar "c") (iVar "c")),
             ("Q", dSInf "Q" (iVar "c") (iVar "c"))]


dSInf n r c = mOpSymInfo arg doubleFloat $ layout r c (iVar (n ++ "_rs")) (iVar (n ++ "_cs"))

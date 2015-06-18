module Core.MatrixOperationTests(allMatrixOperationTests) where

import Control.Monad.Except
import Data.List as L

import Core.IndexExpression
import Core.MatrixOperation
import Core.MOpSyntax
import Core.SymbolTable
import TestUtils.Module
import FrontEnd.Token

allMatrixOperationTests = do
  testFunction matrixOperationToMOp matOpToMOpCases
  testFunction (\e -> simplifySymtab e simpleSt) simpleStCases
  testFunction (\stmt -> typeCheckStmt stmt simpleSt) simpleStmtCases
  testFunction (\stmt -> typeCheckStmt stmt complexSt) complexStmtCases
  testFunction (\stmt -> typeCheckFailed stmt simpleFailSt) simpleFailCases

matOpToMOpCases =
  [(dMatrixOperation "nothing" [] [], mOp "nothing" (mOpSymtab []) []),
   (dMatrixOperation "arg" symList [], mOp "arg" allConstSt []),
   (dMatrixOperation "oneOp" symList [dMatAsg "C" (dMatrixSub (dMatName "A") (dMatName "B"))],
    mOp "oneOp" allConstSt [msub "A" "B" "tmp0", masg "tmp0" "C"]),
   (dMatrixOperation "oneOp" symList [dMatAsg "C" (dMatrixAdd (dMatName "A") (dMatName "B"))],
    mOp "oneOp" allConstSt [madd "A" "B" "tmp0", masg "tmp0" "C"]),
   (dMatrixOperation "oneSMul" symList [dMatAsg "A" (dScalarMul (dMatName "alpha") (dMatName "A"))],
    mOp "oneSMul" allConstSt [msmul "alpha" "A" "tmp0", masg "tmp0" "A"]),
   (dMatrixOperation "oneMMul" symList [dMatAsg "P" (dMatrixMul (dMatName "P") (dMatName "Q"))],
    mOp "oneMMul" mmulSt [mset "tmp0" (mOpDouble 0.0), mmul "P" "Q" "tmp0", masg "tmp0" "P"]),
   (dMatrixOperation "oneTrans" symList [dMatAsg "PT" (dMatrixTrans (dMatName "P"))],
    mOp "oneTrans" mTransSt [mtrans "P" "tmp0", masg "tmp0" "PT"])]

mTransSt =
  addMOpEntry "tmp0" (mOpSymInfo local singleFloat $ layout (iVar "n") (iVar "m") (iConst 1) (iVar "n")) $ mOpSymtab symList

mmulSt =
  addMOpEntry "tmp0" (mOpSymInfo local singleFloat $ layout (iVar "m") (iVar "k") (iConst 1) (iVar "m")) $ mOpSymtab symList

allConstSt =
  addMOpEntry "tmp0" (scSInfLoc (iConst 17) (iConst 17) (iConst 1) (iConst 124)) $ mOpSymtab symList

symList = [("A", scSInf (iConst 17) (iConst 17) (iConst 1) (iConst 124)),
           ("B", scSInf (iConst 17) (iConst 17) (iConst 1) (iConst 124)),
           ("C", scSInf (iConst 17) (iConst 17) (iConst 1) (iConst 124)),
           ("Q", sSInf "Q" (iVar "n") (iVar "k")),
           ("P", sSInf "P" (iVar "m") (iVar "n")),
           ("PT", sSInf "PT" (iVar "n") (iVar "m")),
           ("alpha", scSInf (iConst 1) (iConst 1) (iConst 1) (iConst 1)),
           ("tmp0", scSInfLoc (iConst 17) (iConst 17) (iConst 1) (iConst 124))]
  
simpleStCases =
  L.map (\(x, y) -> (x, Right y))
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
  L.map (\(x, y) -> (x, Right y))
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
  L.map (\(x, y) -> (x, Right y))
  [(dMatAsg "A" (dMatrixMul (dMatName "P") (dMatName "Q")), dupCondSt)]

complexSt =
  mOpSymtab [("A", dSInf "A" (iVar "a") (iVar "b")),
             ("P", dSInf "P" (iVar "c") (iVar "b")),
             ("Q", dSInf "Q" (iVar "b") (iVar "a"))]

dupCondSt =
  mOpSymtab [("A", dSInf "A" (iVar "c") (iVar "c")),
             ("P", dSInf "P" (iVar "c") (iVar "c")),
             ("Q", dSInf "Q" (iVar "c") (iVar "c"))]

typeCheckFailed stmt st =
  case typeCheckStmt stmt st of
    Left err -> Left "fail"
    Right st -> Right st

simpleFailSt =
  mOpSymtab [("A", dSInf "A" (iVar "m") (iVar "n")),
             ("K", dSInf "K" (iConst 12) (iVar "n")),
             ("L", dSInf "L" (iConst 14) (iVar "n"))]

simpleFailCases =
  [(dMatAsg "A" (dMatName "X"), Left "fail"),
   (dMatAsg "A" (dMatrixAdd (dMatName "K") (dMatName "L")), Left "fail"),
   (dMatAsg "A" (dMatrixMul (dMatrixTrans (dMatName "K")) (dMatName "L")), Left "fail")]

dSInf n r c = mOpSymInfo arg doubleFloat $ layout r c (iVar (n ++ "_rs")) (iVar (n ++ "_cs"))
sSInf n r c = mOpSymInfo arg singleFloat $ layout r c (iVar (n ++ "_rs")) (iVar (n ++ "_cs"))

scSInf r c rStride cStride = mOpSymInfo arg singleFloat $ layout r c rStride cStride
scSInfLoc r c rStride cStride = mOpSymInfo local singleFloat $ layout r c rStride cStride
sSInfLoc n r c = mOpSymInfo local singleFloat $ layout r c (iVar (n ++ "_rs")) (iVar (n ++ "_cs"))

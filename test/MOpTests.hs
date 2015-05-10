module MOpTests(allMOpTests) where

import Data.List as L
import Data.Map as M

import EvaluationResult
import IndexExpression
import MiniOperation
import MOpSyntax
import RuntimeEvaluation
import SymbolTable
import Syntax
import TestHarness

allMOpTests = do
  testConvertToMini

testConvertToMini = do
  testConvert "matrix_add" maddSC maddOp
  testConvert "matrix_sub" msubSC msubOp
  testConvert "matrix_trans" mtransSC mtransOp
  testConvert "matrix_set" msetSC msetOp
  testConvert "matrix_smul" msmulSC msmulOp
  testConvert "matrix_multiply" mmmulSC mmmulOp
  testConvert "matrix_assign" masgSC masgOp

testConvert :: String -> Operation String -> MOp -> IO ()
testConvert opName scImpl op =
  let resOp = convertToMini op in
  do
    rtRes <- timeImplementationsFixedSizes "" opName (Just scImpl) [resOp]
    case L.and $ L.map (\(n, evalRes) -> passedSanityCheck evalRes) $ M.toList rtRes of
      True -> putStrLn "test passed"
      False -> putStrLn $ opName ++ " test FAILED"

masgOp =
  mOp "one_matrix_assign" masgOpSym [masg "a" "b"]

masgOpSym = mOpSymtab [("a", mOpSymInfo arg doubleFloat argLayout),
                       ("b", mOpSymInfo arg doubleFloat argLayout)]

mmmulOp =
  mOp "one_matrix_multiply" mmmulOpSym [mmul "a" "b" "c"]

mmmulOpSym =
  mOpSymtab [("a", mOpSymInfo arg singleFloat (layout (iConst 32) (iConst 31) (iConst 1) (iConst 32))),
             ("b", mOpSymInfo arg singleFloat (layout (iConst 31) (iConst 33) (iConst 1) (iConst 31))),
             ("c", mOpSymInfo arg singleFloat (layout (iConst 32) (iConst 33) (iConst 1) (iConst 32)))]

msmulOp =
  mOp "one_matrix_smul" msmulOpSym [msmul "alpha" "b" "c"]

msmulOpSym =
  mOpSymtab [("alpha", mOpSymInfo arg singleFloat (layout (iConst 1) (iConst 1) (iConst 1) (iConst 1))),
             ("b", mOpSymInfo arg singleFloat (layout (iConst 13) (iConst 12) (iConst 12) (iConst 1))),
             ("c", mOpSymInfo arg singleFloat (layout (iConst 13) (iConst 12) (iConst 12) (iConst 1)))]

msetOp =
  mOp "one_matrix_set" msetOpSym [mset "a" (mOpFloat 0.0)]

msetOpSym = mOpSymtab [("a", mOpSymInfo arg singleFloat (layout (iConst 10) (iConst 3) (iConst 3) (iConst 1)))]

mtransOp =
  mOp "one_matrix_transpose" mtransOpSym [mtrans "a" "b"]

mtransOpSym = mOpSymtab [("a", mOpSymInfo arg doubleFloat (layout (iConst 17) (iConst 23) (iConst 5) (iConst 9))),
                         ("b", mOpSymInfo arg doubleFloat (layout (iConst 23) (iConst 17) (iConst 12) (iConst 2)))]

maddOp =
  mOp "one_matrix_add" maddOpSym [madd "a" "b" "c"]

msubOp =
  mOp "one_matrix_subtract" maddOpSym [msub "a" "b" "c"]

argLayout = layout (iConst 8) (iConst 4) (iConst 1) (iConst 8)

maddOpSym = mOpSymtab [("a", mOpSymInfo arg doubleFloat argLayout),
                       ("b", mOpSymInfo arg doubleFloat argLayout),
                       ("c", mOpSymInfo arg doubleFloat argLayout)]

maddSC =
  operation "madd_manual_sc" maddSym $
            block [for "i" (iConst 0) (iConst 1) (iConst 7) (block [innerFor]) ""]

innerFor = for "j" (iConst 0) (iConst 1) (iConst 3) (block maddBodyStmts) ""

maddBodyStmts =
  [load "a_r" "a" (iAdd (iMul (iConst 1) (iVar "i")) (iMul (iConst 8) (iVar "j"))) "",
   load "b_r" "b" (iAdd (iMul (iConst 1) (iVar "i")) (iMul (iConst 8) (iVar "j"))) "",
   plus "b_r" "a_r" "b_r" "",
   store "c" (iAdd (iMul (iConst 1) (iVar "i")) (iMul (iConst 8) (iVar "j"))) "b_r" ""]

maddSym =
  miniSymtab [("a", symInfo (buffer double $ iConst 24) arg),
              ("b", symInfo (buffer double $ iConst 24) arg),
              ("c", symInfo (buffer double $ iConst 24) arg),
              ("a_r", symInfo (sReg double) local),
              ("b_r", symInfo (sReg double) local),
              ("i", symInfo index local),
              ("j", symInfo index local)]

msubSC =
  operation "madd_manual_sc" maddSym $
            block [for "i" (iConst 0) (iConst 1) (iConst 7) (block [innerForSub]) ""]

innerForSub = for "j" (iConst 0) (iConst 1) (iConst 3) (block msubBodyStmts) ""

msubBodyStmts =
  [load "a_r" "a" (iAdd (iMul (iConst 1) (iVar "i")) (iMul (iConst 8) (iVar "j"))) "",
   load "b_r" "b" (iAdd (iMul (iConst 1) (iVar "i")) (iMul (iConst 8) (iVar "j"))) "",
   minus "b_r" "a_r" "b_r" "",
   store "c" (iAdd (iMul (iConst 1) (iVar "i")) (iMul (iConst 8) (iVar "j"))) "b_r" ""]

mtransSC =
  operation "mtrans_manual_sc" transSym $
            block [for "i" (iConst 0) (iConst 1) (iConst 16) (block [innerForTrans]) ""]

innerForTrans = for "j" (iConst 0) (iConst 1) (iConst 22) (block mTransBodyStmts) ""

mTransBodyStmts =
  [load "a_r" "a" (iAdd (iMul (iConst 5) (iVar "i")) (iMul (iConst 9) (iVar "j"))) "",
   store "b" (iAdd (iMul (iConst 12) (iVar "j")) (iMul (iConst 2) (iVar "i"))) "a_r" ""]

transSym =
  miniSymtab [("a", symInfo (buffer double $ iConst 391) arg),
              ("b", symInfo (buffer double $ iConst 391) arg),
              ("a_r", symInfo (sReg double) local),
              ("i", symInfo index local),
              ("j", symInfo index local)]

msetSC =
  operation "mset_manual_sc" msetSym $
            block [for "i" (iConst 0) (iConst 1) (iConst 9) (block [innerForSet]) ""]

innerForSet = for "j" (iConst 0) (iConst 1) (iConst 2) (block mSetBodyStmts) ""

mSetBodyStmts =
  [loadConst "a_r" (floatLit 0.0) "",
   store "a" (iAdd (iMul (iConst 3) (iVar "i")) (iMul (iConst 1) (iVar "j"))) "a_r" ""]

msetSym =
  miniSymtab [("a", symInfo (buffer single $ iConst 391) arg),
              ("a_r", symInfo (sReg single) local),
              ("i", symInfo index local),
              ("j", symInfo index local)]
            
msmulSC =
  operation "madd_manual_sc" smulSym $
            block [for "i" (iConst 0) (iConst 1) (iConst 12) (block [innerForSMul]) ""]

innerForSMul = for "j" (iConst 0) (iConst 1) (iConst 11) (block smulBodyStmts) ""

smulBodyStmts =
  [load "a_r" "a" (iAdd (iMul (iConst 0) (iVar "i")) (iMul (iConst 0) (iVar "j"))) "",
   load "b_r" "b" (iAdd (iMul (iConst 12) (iVar "i")) (iMul (iConst 1) (iVar "j"))) "",
   times "b_r" "a_r" "b_r" "",
   store "c" (iAdd (iMul (iConst 12) (iVar "i")) (iMul (iConst 1) (iVar "j"))) "b_r" ""]

smulSym =
  miniSymtab [("a", symInfo (buffer single $ iConst 24) arg),
              ("b", symInfo (buffer single $ iConst 24) arg),
              ("c", symInfo (buffer single $ iConst 24) arg),
              ("a_r", symInfo (sReg single) local),
              ("b_r", symInfo (sReg single) local),
              ("i", symInfo index local),
              ("j", symInfo index local)]

mmmulSC =
  operation "matrix_multiply_sc" mmulSym $
            block $ [for "i" (iConst 0) (iConst 1) (iConst 31) (block [cMMulFor]) ""]

cMMulFor =
  for "j" (iConst 0) (iConst 1) (iConst 32) (block [innerMMulFor]) ""

innerMMulFor =
  for "k" (iConst 0) (iConst 1) (iConst 30) (block mmulBody) ""

mmulBody =
    [load "a_r" "a" (iAdd (iMul (iConst 1) (iVar "i")) (iMul (iConst 32) (iVar "k"))) "",
     load "b_r" "b" (iAdd (iMul (iConst 1) (iVar "k")) (iMul (iConst 31) (iVar "j"))) "",
     load "c_r" "c" (iAdd (iMul (iConst 1) (iVar "i")) (iMul (iConst 32) (iVar "j"))) "",
     times "b_r" "a_r" "b_r" "",
     plus "c_r" "b_r" "c_r" "",
     store "c" (iAdd (iMul (iConst 1) (iVar "i")) (iMul (iConst 32) (iVar "j"))) "c_r" ""]

mmulSym =
  miniSymtab [("a", symInfo (buffer single $ iConst (31*32)) arg),
              ("b", symInfo (buffer single $ iConst (31*33)) arg),
              ("c", symInfo (buffer single $ iConst (32*33)) arg),
              ("a_r", symInfo (sReg single) local),
              ("b_r", symInfo (sReg single) local),
              ("c_r", symInfo (sReg single) local),
              ("i", symInfo index local),
              ("j", symInfo index local),
              ("k", symInfo index local)]

masgSC =
  operation "masg_manual_sc" masgSym $
            block [for "i" (iConst 0) (iConst 1) (iConst 7) (block [innerAsgFor]) ""]

innerAsgFor = for "j" (iConst 0) (iConst 1) (iConst 3) (block masgBodyStatements) ""

masgBodyStatements =
  [load "a_r" "a" (iAdd (iMul (iConst 1) (iVar "i")) (iMul (iConst 8) (iVar "j"))) "",
   store "b" (iAdd (iMul (iConst 1) (iVar "i")) (iMul (iConst 8) (iVar "j"))) "a_r" ""]

masgSym =
  miniSymtab [("a", symInfo (buffer double $ iConst 24) arg),
              ("b", symInfo (buffer double $ iConst 24) arg),
              ("a_r", symInfo (sReg double) local),
              ("i", symInfo index local),
              ("j", symInfo index local)]

module MOpTests() where

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

testConvertToMini = do
  testConvert "matrix_add" maddSC maddOp
  testConvert "matrix_sub" msubSC msubOp

testConvert :: String -> Operation String -> MOp -> IO ()
testConvert opName scImpl op =
  let resOp = convertToMini op in
  do
    rtRes <- timeImplementations "" opName (Just scImpl) [resOp]
    case L.and $ L.map (\(n, evalRes) -> passedSanityCheck evalRes) $ M.toList rtRes of
      True -> putStrLn "test passed"
      False -> putStrLn $ opName ++ " test FAILED"

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


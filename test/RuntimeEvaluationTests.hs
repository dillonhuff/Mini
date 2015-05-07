module RuntimeEvaluationTests() where

import Data.Map as M

import IndexExpression
import MiniOperation
import RuntimeEvaluation
import SymbolTable
import Syntax

compileGenSizeTest =
  timeImplementations
        dimSizes
        ""
        "matrix_assign"
        (Just $ operation "copy_sc" testGenSizeSymtab (block [masgLoops]))
        [operation "copy_op" testGenSizeSymtab (block [masgLoops])]

dimSizes =
  M.fromList [("a_nrows", 123), ("a_ncols", 12),
              ("a_rs", 12), ("a_cs", 1),
              ("b_rs", 18), ("b_cs", 123)]

copyImp = operation "copy_op" testGenSizeSymtab (block [masgLoops])

testGenSizeSymtab =
  miniSymtab [("a", symInfo (buffer double (iMul (iVar "a_nrows") (iVar "a_ncols"))) arg),
              ("b", symInfo (buffer double (iMul (iVar "a_nrows") (iVar "a_ncols"))) arg),
              ("a_nrows", symInfo index arg),
              ("a_ncols", symInfo index arg),
              ("a_rs", symInfo index arg),
              ("a_cs", symInfo index arg),
              ("b_rs", symInfo index arg),
              ("b_cs", symInfo index arg),
              ("i", symInfo index local),
              ("j", symInfo index local),
              ("a_r", symInfo (sReg double) local)]

masgLoops =
  for "i" (iConst 0) (iConst 1) (iSub (iVar "a_nrows") (iConst 1)) (block [innerFor]) ""

innerFor =
  for "j" (iConst 0) (iConst 1) (iSub (iVar "a_ncols") (iConst 1)) (block masgBodyStmts) ""

masgBodyStmts =
  [load "a_r" "a" (iAdd (iMul (iVar "a_rs") (iVar "i")) (iMul (iVar "a_cs") (iVar "j"))) "",
   store "b" (iAdd (iMul (iVar "b_rs") (iVar "i")) (iMul (iVar "b_cs") (iVar "j"))) "a_r" ""]
   

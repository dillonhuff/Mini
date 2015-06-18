module Optimizations.RegisterSynonymDeletionTests(allRegisterSynonymDeletionTests) where

import Core.IndexExpression
import Core.MiniSyntax
import Optimizations.RegisterSynonymDeletion
import TestUtils.Module

allRegisterSynonymDeletionTests = do
  testFunction deleteRegisterSynonymsFromStmts stmtCases

stmtCases =
  [([ldC], [ldC]),
   ([ldC, raBA], [ldC]),
   ([ldC, raBA, addBQ], [ldC, addBA]),
   ([ldC, raBA, raCB, addCQ], [ldC, addAQ]),
   ([maddLoops maddBodyStmts], [maddLoops maddBodyStmtsAfterDeletion]),
   ([vaddLoop vaddBody], [vaddLoop vaddBodyAfterDeletion]),
   ([vaddLoop vaddBodyOneAsg], [vaddLoop vaddBodyOneAsgAfterDeletion]),
   (vaddBodyOneAsg, vaddBodyOneAsgAfterDeletion)]

ldC = loadConst "a" (floatLit 1.2) "l1"
raBA = regAssign "b" "a" "l2"
raXB = regAssign "x" "b" "l4"
addBQ = plus "p" "b" "q" "l5"
addBA = plus "p" "a" "q" "l5"
addCQ = plus "p" "c" "q" "l6"
addAQ = plus "p" "a" "q" "l6"
raCB = regAssign "c" "b" "l7"

maddLoops innerStmts =
  for "i" (iConst 0) (iConst 1) (iVar "BNRows") (block [maddInnerLoop innerStmts]) "$outer"

maddInnerLoop innerStmts =
  for "j" (iConst 0) (iConst 1) (iVar "BNCols") (block innerStmts) "$inner"

maddBodyStmts =
  [load "a" "A" (iAdd (iMul (iVar "A_rs") (iVar "i")) (iMul (iVar "A_cs") (iVar "j"))) "l0",
   load "b" "B" (iAdd (iMul (iVar "B_rs") (iVar "i")) (iMul (iVar "B_cs") (iVar "j"))) "l1",
   plus "tmp1" "a" "b" "l2",
   regAssign "tmp2" "tmp1" "l3",
   regAssign "tmp3" "tmp2" "l4",
   store "B" (iAdd (iMul (iVar "B_rs") (iVar "i")) (iMul (iVar "B_cs") (iVar "j"))) "tmp3" "l5"]

maddBodyStmtsAfterDeletion =
  [load "a" "A" (iAdd (iMul (iVar "A_rs") (iVar "i")) (iMul (iVar "A_cs") (iVar "j"))) "l0",
   load "b" "B" (iAdd (iMul (iVar "B_rs") (iVar "i")) (iMul (iVar "B_cs") (iVar "j"))) "l1",
   plus "tmp1" "a" "b" "l2",
   store "B" (iAdd (iMul (iVar "B_rs") (iVar "i")) (iMul (iVar "B_cs") (iVar "j"))) "tmp1" "l5"]

vaddLoop bodyStmts =
  for "i" (iConst 0) (iConst 1) (iVar "ANumRows") (block bodyStmts) "$l"

vaddBody =
  [load "a" "A" (iVar "i") "l0",
   load "b" "B" (iVar "i") "l1",
   plus "tmp1" "a" "b" "l2",
   regAssign "tmp2" "tmp1" "l3",
   regAssign "tmp3" "tmp2" "l4",
   store "A" (iVar "i") "tmp3" "l5"]

vaddBodyAfterDeletion =
  [load "a" "A" (iVar "i") "l0",
   load "b" "B" (iVar "i") "l1",
   plus "tmp1" "a" "b" "l2",
   store "A" (iVar "i") "tmp1" "l5"]

vaddBodyOneAsg =
  [load "a" "A" (iVar "i") "l0",
   load "b" "B" (iVar "i") "l1",
   plus "tmp1" "a" "b" "l2",
   regAssign "tmp2" "tmp1" "l3",
   store "A" (iVar "i") "tmp2" "l5"]

vaddBodyOneAsgAfterDeletion =
  [load "a" "A" (iVar "i") "l0",
   load "b" "B" (iVar "i") "l1",
   plus "tmp1" "a" "b" "l2",
   store "A" (iVar "i") "tmp1" "l5"]

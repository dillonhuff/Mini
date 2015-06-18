module Optimizations.RegisterSynonymDeletionTests(allRegisterSynonymDeletionTests) where

import Core.IndexExpression
import Core.MiniSyntax
import Optimizations.RegisterSynonymDeletion
import TestUtils.Dummies.Loop
import TestUtils.Module

allRegisterSynonymDeletionTests = do
  testFunction deleteRegisterSynonymsFromStmts stmtCases

stmtCases =
  [([cA], [cA]),
   ([cA, rBA], [cA]),
   ([cA, rBA, addBQ], [cA, addBA]),
   ([cA, rBA, raCB, aPCQ], [cA, addAQ]),
   ([maddLoops maddBodyStmts], [maddLoops maddBodyStmtsAfterDeletion]),
   ([vaddLoop vaddBody], [vaddLoop vaddBodyAfterDeletion]),
   ([vaddLoop vaddBodyOneAsg], [vaddLoop vaddBodyOneAsgAfterDeletion]),
   (vaddBodyOneAsg, vaddBodyOneAsgAfterDeletion)]

cA = loadConst "a" (floatLit 1.2) "l1"
rBA = regAssign "b" "a" "l2"
raXB = regAssign "x" "b" "l4"
addBQ = plus "p" "b" "q" "l5"
addBA = plus "p" "a" "q" "l5"
aPCQ = plus "p" "c" "q" "l6"
addAQ = plus "p" "a" "q" "l6"
raCB = regAssign "c" "b" "l7"

maddLoops innerStmts =
  p2S0I1ES "i" "BNRows" "j" "BNCols" innerStmts

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
  pS0I1ES "i" "ANumRows" bodyStmts

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

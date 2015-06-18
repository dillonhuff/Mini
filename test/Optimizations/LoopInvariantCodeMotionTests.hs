module Optimizations.LoopInvariantCodeMotionTests(allLoopInvariantCodeMotionTests) where

import Core.IndexExpression
import Core.MiniSyntax
import Optimizations.LoopInvariantCodeMotion
import TestUtils.Dummies.Loop
import TestUtils.Module

allLoopInvariantCodeMotionTests = do
  testFunction pullConstantLoadsOutOfLoops constantLoadCases

constantLoadCases =
  [([ldC], [ldC]),
   ([svmulLoop svmulBody], [ldAlpha, svmulLoop svmulBodyAfterLift]),
   ([smmulLoops smmulBody], [ldAlpha, smmulLoops smmulBodyAfterLift])]

ldC = loadConst "a" (floatLit 1.2) "l1"

svmulLoop bodyStmts =
  pS0I1ES "i" "ANumRows" bodyStmts

svmulBody =
  [load "a" "A" (iVar "i") "l0",
   load "alpha1" "alpha" (iConst 0) "l1",
   times "tmp1" "alpha" "a" "l2",
   store "A" (iVar "i") "tmp1" "l5"]

svmulBodyAfterLift =
  [load "a" "A" (iVar "i") "l0",
   times "tmp1" "alpha" "a" "l2",
   store "A" (iVar "i") "tmp1" "l5"]

ldAlpha = load "alpha1" "alpha" (iConst 0) "l1"

smmulLoops innerStmts =
  p2S0I1ES "i" "BNRows" "j" "BNCols" innerStmts

smmulBody =
  [load "a" "A" (iAdd (iMul (iVar "A_rs") (iVar "i")) (iMul (iVar "A_cs") (iVar "j"))) "l0",
   ldAlpha,
   times "tmp1" "a" "alpha" "l2",
   store "A" (iAdd (iMul (iVar "A_rs") (iVar "i")) (iMul (iVar "A_cs") (iVar "j"))) "tmp1" "l5"]

smmulBodyAfterLift =
  [load "a" "A" (iAdd (iMul (iVar "A_rs") (iVar "i")) (iMul (iVar "A_cs") (iVar "j"))) "l0",
   times "tmp1" "a" "alpha" "l2",
   store "A" (iAdd (iMul (iVar "A_rs") (iVar "i")) (iMul (iVar "A_cs") (iVar "j"))) "tmp1" "l5"]

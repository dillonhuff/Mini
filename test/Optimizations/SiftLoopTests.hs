module Optimizations.SiftLoopTests(allSiftLoopTests) where

import Core.IndexExpression
import Core.MiniSyntax
import Optimizations.SiftLoops
import TestUtils

allSiftLoopTests = do
  testFunction siftLoopsInStmtList siftTestCases

siftTestCases =
  [([], []),
   ([ldC], [ldC]),
   ([emptyLoop, ldC], [ldC, emptyLoop]),
   ([oneLoop, ldC, ra], [ldC, ra, oneLoop]),
   ([oneLoop, ldC, ra, emptyLoop], [ldC, ra, oneLoop, emptyLoop]),
   ([oneLoop, ldC, rk], [oneLoop, ldC, rk]),
   ([oneLoop, ldC, emptyLoop, ra], [ldC, ra, oneLoop, emptyLoop])]

emptyLoop =
  for "i" (iConst 0) (iConst 1) (iVar "n") (block simpleStmts) "empty-loop"

simpleStmts = []

oneLoop =
  for "i" (iConst 0) (iConst 1) (iVar "n") (block oneStmts) "empty-loop"

oneStmts =
  [loadConst "x" (floatLit 2.3) "l9",
   loadConst "y" (floatLit 3.4) "l10",
   plus "x" "y" "z" "l11",
   times "k" "l" "y" "l12"]

ldC = loadConst "a" (floatLit 0.0) "l5"
ra = regAssign "b" "a" "l6"
rk = regAssign "a" "k" "l7"

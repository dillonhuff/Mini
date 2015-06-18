module Analysis.RegisterLivenessTests(allRegisterLivenessTests) where

import Analysis.Liveness.Register
import Core.MiniSyntax
import TestUtils.Module

allRegisterLivenessTests = do
  testFunction regLiveRange liveRangeCases

regLiveRange regName =
  liveRange regName regRanges

liveRangeCases =
  [("r1", (0, 0)),
   ("r2", (1, 3)),
   ("r3", (2, 3)),
   ("r4", (3, 3))]

regRanges = liveRanges testStmts

testStmts =
  [loadConst "r1" (doubleLit 1.0) 0,
   loadConst "r2" (doubleLit 2.0) 1,
   plus "r3" "r2" "r2" 2,
   times "r4" "r3" "r2" 3]

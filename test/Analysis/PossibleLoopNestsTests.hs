module Analysis.PossibleLoopNestsTests(allPossibleLoopNestsTests) where

import Data.List as L
import Data.Set as S

import Analysis.Loop
import Core.IndexExpression
import Core.MiniSyntax
import TestUtils

allPossibleLoopNestsTests =
  testFunction genPossibleLoopNests loopNestCases

loopNestCases =
  L.map (\(x, y) -> (x, S.fromList y))
  [(ldA, [ldA]),
   (pLoop "i" $ bLoop "j" [ldA],
    [pLoop "i" $ bLoop "j" [ldA], pLoop "j" $ bLoop "i" [ldA]])]

ldA = loadConst "a" (doubleLit 1.0) "l1"

genPossibleLoopNests stmts =
  let actual = possibleLoopOrderingsForPerfectNest stmts in
  S.fromList actual

pLoop indVar innerLoop =
  for indVar (iConst 0) (iConst 1) (iVar "n") (block [innerLoop]) "dummyLabel"
  
bLoop indVar body =
  for indVar (iConst 0) (iConst 1) (iVar "n") (block body) "dummyLabel"

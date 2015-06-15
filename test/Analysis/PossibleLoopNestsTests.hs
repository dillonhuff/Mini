module Analysis.PossibleLoopNestsTests(allPossibleLoopNestsTests) where

import Data.List as L
import Data.Set as S

import Analysis.Loop
import Core.IndexExpression
import Core.MiniSyntax
import TestUtils

allPossibleLoopNestsTests = do
  testFunction genPossibleLoopNests loopNestCases
  testFunction genPossibleImpls implCases

loopNestCases =
  L.map (\(x, y) -> (x, S.fromList y))
  [(ldA, [ldA]),
   (pLoop "i" $ bLoop "j" [ldA],
    [pLoop "i" $ bLoop "j" [ldA], pLoop "j" $ bLoop "i" [ldA]])]

implCases =
  L.map (\(x, y) -> (x, S.fromList y))
  [([ldA], [[ldA]]),
   ([pLoop "i" $ bLoop "j" [ldA],
     pLoop "k" $ bLoop "l" [ldA]],
    
    [[pLoop "i" $ bLoop "j" [ldA],
      pLoop "k" $ bLoop "l" [ldA]],
     
     [pLoop "j" $ bLoop "i" [ldA],
      pLoop "k" $ bLoop "l" [ldA]],
     
     [pLoop "i" $ bLoop "j" [ldA],
      pLoop "l" $ bLoop "k" [ldA]],
     
     [pLoop "j" $ bLoop "i" [ldA],
      pLoop "l" $ bLoop "k" [ldA]]])]

ldA = loadConst "a" (doubleLit 1.0) "l1"

genPossibleLoopNests stmt =
  let actual = possibleLoopOrderingsForPerfectNest stmt in
  S.fromList actual

genPossibleImpls stmts =
  let actual = possibleLoopOrderingsForPerfectNests stmts in
  S.fromList actual

pLoop indVar innerLoop =
  for indVar (iConst 0) (iConst 1) (iVar "n") (block [innerLoop]) "dummyLabel"
  
bLoop indVar body =
  for indVar (iConst 0) (iConst 1) (iVar "n") (block body) "dummyLabel"

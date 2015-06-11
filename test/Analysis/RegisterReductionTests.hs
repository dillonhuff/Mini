module Analysis.RegisterReductionTests(allRegisterReductionTests) where

import Analysis.Dependence.Graph
import Analysis.Dependence.RegisterReduction
import Core.IndexExpression
import Core.MiniSyntax
import TestUtils

allRegisterReductionTests = do
  testFunction (rrDep flowDependent) flowCases
  testFunction (rrDep antiDependent) antiCases
  testFunction (rrDep outputDependent) outCases
  testFunction (loopDep flowDependent) loopFlowCases
  testFunction (loopDep antiDependent) loopAntiCases
  testFunction (loopDep outputDependent) loopOutputCases
  testFunction (perfectDoubleLoopNest flowDependent) perfectDoubleLoopFlowCases

flowCases =
  [(("l1", "l2"), False),
   (("l4", "l1"), True),
   (("l5", "l3"), True),
   (("l6", "l5"), True),
   (("l5", "l6"), False),
   (("l8", "l6"), True)]

antiCases =
  [(("l1", "l6"), False),
   (("l7", "l4"), True),
   (("l4", "l7"), False)]

outCases =
  [(("l2", "l6"), False),
   (("l5", "l4"), True),
   (("l8", "l7"), True)]
  
rrDep f (t, s) = f depGraph t s

depGraph =
  case buildDependenceGraph simpleStmts of
    Just dg -> dg
    Nothing -> error $ "Cannot build depGraph with " ++ show simpleStmts

simpleStmts =
  [loadConst "a" (floatLit 1.2) "l1",
   loadConst "x" (floatLit 2.0) "l2",
   load "alpha1" "alpha" (iConst 0) "l3",
   plus "q" "a" "x" "l4",
   times "q" "alpha1" "q" "l5",
   store "B" (iVar "i") "q" "l6",
   minus "a" "x" "q" "l7",
   load "a" "B" (iVar "i") "l8"]

loopDep f (t, s) = f loopDepGraph t s

loopDepGraph =
  case buildDependenceGraph loopStmts of
    Just dg -> dg
    Nothing -> error $ "Cannot build depGraph with " ++ show loopStmts

loopFlowCases =
  [(("l1", "l2"), False),
   (("l4", "l2"), True),
   (("l9", "l7"), True)]

loopAntiCases =
  [(("l1", "l3"), False),
   (("l7", "l9"), True),
   (("l1", "l4"), True)]

loopOutputCases =
  [(("l10", "l10"), False),
   (("l8", "l8"), True)]

loopStmts = [for "i" (iConst 0) (iConst 1) (iVar "n") (block loopStmtsBody) "l0"]

loopStmtsBody = [load "tmp031" "tmp0" (iVar "i") "l1",
		 load "y33" "y" (iVar "i") "l2",
		 load "tmp135" "tmp1" (iConst 0) "l3",
		 times "tmp137" "tmp031" "y33" "l4",
		 plus "tmp138" "tmp135" "tmp137" "l5",
		 store "tmp1" (iConst 0) "tmp138" "l6",
		 load "alpha11" "alpha" (iConst 0) "l7",
		 load "y13" "y" (iVar "i") "l8",
		 times "tmp24" "alpha11" "y13" "l9",
		 store "z" (iVar "i") "tmp24" "l10"]


perfectDoubleLoopNest f (t, s) = f perfectDoubleLoopGraph t s

perfectDoubleLoopGraph =
  case buildDependenceGraph perfectDoubleLoopStmts of
    Just dg -> dg
    Nothing -> error $ "Cannot build depGraph with " ++ show loopStmts

perfectDoubleLoopFlowCases =
  [(("l2", "l3"), False),
   (("l7", "l6"), True),
   (("l3", "l7"), False),
   (("l2", "l7"), False)]

perfectDoubleLoopStmts = [for "i" (iConst 0) (iConst 1) (iVar "n") (block innerBody) "l0"]
innerBody = [for "j" (iConst 0) (iConst 1) (iVar "m") (block doubleLoopStmts) "l1"]
doubleLoopStmts = [load "alpha11" "alpha" (iConst 0) "l2",
		   load "A13" "A" (iAdd (iMul (iVar "A_rs") (iVar "i")) (iVar "j")) "l3",
                   times "tmp015" "alpha11" "A13" "l4",
                   store "tmp0" (iAdd (iMul (iVar "A_rs") (iVar "i")) (iVar "j")) "tmp015" "l5",
                   load "tmp04" "tmp0" (iAdd (iMul (iVar "A_rs") (iVar "i")) (iVar "j")) "l6",
                   store "A" (iAdd (iMul (iVar "A_rs") (iVar "i")) (iVar "j")) "tmp04" "l7"]


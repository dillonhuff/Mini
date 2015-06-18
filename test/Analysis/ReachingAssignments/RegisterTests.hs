module Analysis.ReachingAssignments.RegisterTests(allReachingAssignmentsTests) where

import Data.List as L
import Data.Map as M

import Analysis.ReachingAssignments.Register
import Core.MiniSyntax
import TestUtils.Module

allReachingAssignmentsTests = do
  testFunction reachingAssignments regSynonymCases

regSynonymCases =
  L.map (\(x, y) -> (x, M.fromList y))
  [([ldC], [("l1", [])]),
   ([ldC, raBA], [("l1", []), ("l2", [raBA])]),
   ([ldC, raBA, raAC], [("l1", []), ("l2", [raBA]), ("l3", [raAC])]),
   ([ldC, raBA, raXB], [("l1", []), ("l2", [raBA]), ("l4", [raXB, raBA])])]

ldC = loadConst "a" (doubleLit 1.0) "l1"
raBA = regAssign "b" "a" "l2"
raAC = regAssign "a" "c" "l3"
raXB = regAssign "x" "b" "l4"

module Analysis.ReachingAssignments.RegisterTests(allReachingAssignmentsTests) where

import Data.List as L
import Data.Map as M

import Analysis.ReachingAssignments.Register
import Core.MiniSyntax
import TestUtils.Dummies.Statement
import TestUtils.Module

allReachingAssignmentsTests = do
  testFunction reachingAssignments regSynonymCases

regSynonymCases =
  L.map (\(x, y) -> (x, M.fromList y))
  [([cA], [(label cA, [])]),
   ([cA, rBA], [(label cA, []), (label rBA, [rBA])]),
   ([cA, rBA, rAC], [(label cA, []), (label rBA, [rBA]), (label rAC, [rAC])]),
   ([cA, rBA, rXB], [(label cA, []), (label rBA, [rBA]), (label rXB, [rXB, rBA])])]


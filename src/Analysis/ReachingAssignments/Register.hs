module Analysis.ReachingAssignments.Register(reachingAssignments) where

import Data.List as L
import Data.Map as M

import Core.MiniSyntax

reachingAssignments :: (Ord a) => [Statement a] -> Map a [Statement a]
reachingAssignments [] = M.empty
reachingAssignments stmts =
  M.fromList $ L.map reachingAssignmentsAt $ L.tail $ L.inits stmts

reachingAssignmentsAt stmts =
  let lastLabel = label $ L.head $ L.reverse stmts in
  (lastLabel, L.foldl updateReachingAssignments [] stmts)

updateReachingAssignments :: [Statement a] -> Statement a -> [Statement a]
updateReachingAssignments assignsSoFar stmt =
  let newAssigns = L.filter (\asg -> not $ L.elem (operandWritten stmt) (allOperands asg)) assignsSoFar in
  case isRegAssign stmt of
    True -> stmt : newAssigns
    False -> newAssigns

module Analysis.RegisterSynonyms.Register(registerSynonyms) where

import Data.List as L
import Data.Map as M
import Data.Maybe

import Analysis.Basic
import Analysis.ReachingAssignments.Register
import Core.MiniSyntax
import Utils.MapUtils

registerSynonyms :: (Show a, Ord a) => [Statement a] -> Map Operand Operand
registerSynonyms stmts =
  let reachingAsgs = reachingAssignments stmts
      regUseLocationPairs = M.toList $ registerUsageLocations $ block stmts in
  M.fromList $ catMaybes $ L.map (\(reg, useLocs) -> regSynonymPair reg useLocs reachingAsgs) regUseLocationPairs

regSynonymPair :: (Show a, Ord a) => Operand -> [a] -> Map a [Statement a] -> Maybe (Operand, Operand)
regSynonymPair reg useLocs reachingAsgs = do
  regValuesAtEachUse <- sequence $ L.map (regValueAt reg reachingAsgs) useLocs
  case L.length (L.nub regValuesAtEachUse) == 1 of
    True -> Just (reg, head regValuesAtEachUse)
    False -> Nothing

regValueAt reg reachingAssigns label =
  let assigns = L.filter isRegAssign $ lookupF label reachingAssigns
      assignsToReg = L.filter (\asg -> operandWritten asg == reg) assigns in
  case L.length assignsToReg == 1 of
    True -> Just $ head $ operandsRead $ head assignsToReg
    False -> Nothing
